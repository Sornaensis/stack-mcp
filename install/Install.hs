module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (when, unless)
import Data.List (intercalate, isInfixOf, isSuffixOf)
import System.Directory
  ( copyFile
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getCurrentDirectory
  , getHomeDirectory
  , getXdgDirectory
  , listDirectory
  , XdgDirectory(..)
  )
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath ((</>), takeDirectory)
import System.Info (os)
import System.Process (readCreateProcessWithExitCode, proc, CreateProcess(..))

main :: IO ()
main = do
  args <- getArgs
  let target = case args of
        (t:_) | t `elem` ["all", "claude", "copilot"] -> t
        []    -> "all"
        _     -> error "Usage: stack-mcp-install [all|claude|copilot]"

  putStrLn "=== stack-mcp installer ==="

  -- Build the executable
  putStrLn "\nBuilding stack-mcp..."
  projectDir <- getCurrentDirectory
  (ec, _, serr) <- readCreateProcessWithExitCode
    (proc "stack" ["build"]) { cwd = Just projectDir } ""
  case ec of
    ExitSuccess   -> pure ()
    ExitFailure _ -> do
      -- stack build can fail at the copy/register step even though
      -- compilation succeeded (e.g. locked exe on Windows).
      -- We check for the built exe below and only fail if it's missing.
      putStrLn "  Warning: stack build exited with an error (copy step may have failed)"
      putStrLn $ "  " ++ lastLine serr

  -- Resolve the built executable path
  exePath <- resolveExePath
  putStrLn $ "stack-mcp installed at: " ++ exePath

  -- Install configs
  when (target == "all" || target == "claude") $ do
    putStrLn "\nConfiguring Claude Code..."
    dest <- claudeConfigPath
    installConfig "mcpServers" dest exePath

  when (target == "all" || target == "copilot") $ do
    putStrLn "\nConfiguring VS Code Copilot..."
    dest <- copilotConfigPath
    installConfig "servers" dest exePath
    putStrLn "\nInstalling agents..."
    installAgents projectDir

  putStrLn "\n=== Installation complete ==="
  putStrLn "Restart Claude Code / VS Code to pick up the new MCP server."

------------------------------------------------------------------------
-- Executable resolution
------------------------------------------------------------------------

resolveExePath :: IO FilePath
resolveExePath = do
  -- Find the exe in the dist build directory (pre-copy location)
  -- This works even if the copy/register step failed
  (ec, sout, _) <- readCreateProcessWithExitCode
    (proc "stack" ["path", "--dist-dir"]) ""
  let distDir = strip sout
  case ec of
    ExitSuccess -> do
      let builtExe = distDir </> "build" </> "stack-mcp" </> exeName
      exists <- doesFileExist builtExe
      unless exists $ do
        putStrLn $ "ERROR: stack-mcp not found at " ++ builtExe
        putStrLn   "       Compilation may have failed — check the build output above."
        exitFailure
      -- Kill any running instances so the exe isn't locked
      killRunningInstances
      -- Copy to the install bin directory
      destDir <- installBinDir
      createDirectoryIfMissing True destDir
      let destExe = destDir </> exeName
      putStrLn $ "  Copying to " ++ destExe
      copyFile builtExe destExe
      pure destExe
    _ -> do
      putStrLn "ERROR: could not determine stack dist dir"
      exitFailure
  where
    exeName = if isWindows then "stack-mcp.exe" else "stack-mcp"

-- | Target install directory for the stack-mcp binary.
-- Windows: %APPDATA%/stack-mcp/bin/
-- Unix:    ~/.local/bin/
installBinDir :: IO FilePath
installBinDir
  | isWindows = do
      appData <- getXdgDirectory XdgData ""
      pure $ appData </> "stack-mcp" </> "bin"
  | otherwise = do
      home <- getHomeDirectory
      pure $ home </> ".local" </> "bin"

------------------------------------------------------------------------
-- Kill running instances (so the exe can be overwritten)
------------------------------------------------------------------------

killRunningInstances :: IO ()
killRunningInstances = do
  killed <- if isWindows
    then do
      (ec, _, _) <- readCreateProcessWithExitCode
        (proc "taskkill" ["/F", "/T", "/IM", "stack-mcp.exe"]) ""
      pure (ec == ExitSuccess)
    else do
      (ec, _, _) <- readCreateProcessWithExitCode
        (proc "pkill" ["-f", "stack-mcp"]) ""
      pure (ec == ExitSuccess)
  when killed $ do
    putStrLn "  Killed running stack-mcp processes"
    threadDelay 1000000  -- 1 second for OS to release handles

------------------------------------------------------------------------
-- Config paths (cross-platform)
------------------------------------------------------------------------

claudeConfigPath :: IO FilePath
claudeConfigPath
  | isMacOS = do
      home <- getXdgDirectory XdgData ""
      -- XdgData on macOS = ~/Library/Application Support
      pure $ takeDirectory home </> "Library" </> "Application Support"
             </> "Claude" </> "claude_desktop_config.json"
  | isWindows = do
      appData <- getXdgDirectory XdgData ""
      -- On Windows XdgData ~ AppData/Roaming
      pure $ appData </> "claude" </> "claude_desktop_config.json"
  | otherwise = do
      configDir <- getXdgDirectory XdgConfig ""
      pure $ configDir </> "claude" </> "claude_desktop_config.json"

copilotConfigPath :: IO FilePath
copilotConfigPath
  | isMacOS = do
      home <- getXdgDirectory XdgData ""
      pure $ takeDirectory home </> "Library" </> "Application Support"
             </> "Code" </> "User" </> "mcp.json"
  | isWindows = do
      appData <- getXdgDirectory XdgData ""
      pure $ appData </> "Code" </> "User" </> "mcp.json"
  | otherwise = do
      configDir <- getXdgDirectory XdgConfig ""
      pure $ configDir </> "Code" </> "User" </> "mcp.json"

-- | VS Code Copilot user-level agents directory.
copilotAgentsDir :: IO FilePath
copilotAgentsDir
  | isMacOS = do
      home <- getXdgDirectory XdgData ""
      pure $ takeDirectory home </> "Library" </> "Application Support"
             </> "Code" </> "User" </> "prompts"
  | isWindows = do
      appData <- getXdgDirectory XdgData ""
      pure $ appData </> "Code" </> "User" </> "prompts"
  | otherwise = do
      configDir <- getXdgDirectory XdgConfig ""
      pure $ configDir </> "Code" </> "User" </> "prompts"

------------------------------------------------------------------------
-- Agent installation
------------------------------------------------------------------------

installAgents :: FilePath -> IO ()
installAgents projectDir = do
  let agentsDir = projectDir </> "agents"
  hasAgents <- doesDirectoryExist agentsDir
  unless hasAgents $ do
    putStrLn "  WARNING: agents/ directory not found, skipping"
    pure ()
  when hasAgents $ do
    destDir <- copilotAgentsDir
    createDirectoryIfMissing True destDir
    files <- listDirectory agentsDir
    let agentFiles = filter (".agent.md" `isSuffixOf`) files
    mapM_ (\f -> do
      copyFile (agentsDir </> f) (destDir </> f)
      putStrLn $ "  Installed: " ++ f
      ) agentFiles
    putStrLn $ "  " ++ show (length agentFiles) ++ " agents installed to " ++ destDir

------------------------------------------------------------------------
-- Config merge (upserts stack_mcp into existing config)
------------------------------------------------------------------------

installConfig :: String -> FilePath -> FilePath -> IO ()
installConfig serverKey destPath exePath = do
  createDirectoryIfMissing True (takeDirectory destPath)
  destExists <- doesFileExist destPath
  existing <- if destExists
    then do c <- readFile destPath; length c `seq` pure c
    else pure ""
  let entry = serverEntryValue serverKey exePath
      config = case parseJSON existing of
        Just obj -> upsertServer serverKey entry obj
        Nothing  -> upsertServer serverKey entry (JObj [])
      rendered = renderJSON 0 config ++ "\n"
  writeFile destPath rendered
  putStrLn $ (if destExists then "  Updated: " else "  Wrote: ") ++ destPath

serverEntryValue :: String -> FilePath -> JValue
serverEntryValue key exePath
  | key == "servers" = -- copilot format
      JObj [ ("type", JStr "stdio")
           , ("command", JStr (jsonEscapeRaw exePath))
           , ("args", JArr [])
           ]
  | otherwise = -- claude format
      JObj [ ("command", JStr (jsonEscapeRaw exePath))
           , ("args", JArr [])
           , ("env", JObj [])
           ]

-- | Upsert stack_mcp into the servers/mcpServers object.
upsertServer :: String -> JValue -> JValue -> JValue
upsertServer serverKey entry (JObj kvs) =
  case lookup serverKey kvs of
    Just (JObj servers) ->
      JObj (upsertKV serverKey (JObj (upsertKV "stack_mcp" entry servers)) kvs)
    _ ->
      JObj (upsertKV serverKey (JObj [("stack_mcp", entry)]) kvs)
upsertServer serverKey entry _ =
  JObj [(serverKey, JObj [("stack_mcp", entry)])]

upsertKV :: String -> JValue -> [(String, JValue)] -> [(String, JValue)]
upsertKV key val [] = [(key, val)]
upsertKV key val ((k,v):rest)
  | k == key  = (key, val) : rest
  | otherwise = (k, v) : upsertKV key val rest

-- | Escape a file path for raw JSON string content (double backslashes).
jsonEscapeRaw :: String -> String
jsonEscapeRaw = concatMap (\c -> if c == '\\' then "\\\\" else [c])

------------------------------------------------------------------------
-- Minimal JSON parser / printer (no external dependencies)
------------------------------------------------------------------------

data JValue = JObj [(String, JValue)]
            | JArr [JValue]
            | JStr String    -- raw content between quotes (escapes preserved)
            | JNum String    -- raw number text
            | JBool Bool
            | JNull

parseJSON :: String -> Maybe JValue
parseJSON s = case pValue (skipWS s) of
  Just (v, rest) | all isWS rest -> Just v
  _ -> Nothing

pValue :: String -> Maybe (JValue, String)
pValue ('{':s) = pObj (skipWS s) []
pValue ('[':s) = pArr (skipWS s) []
pValue ('"':s) = do (raw, rest) <- pRawStr s; Just (JStr raw, rest)
pValue ('t':'r':'u':'e':s)  = Just (JBool True, s)
pValue ('f':'a':'l':'s':'e':s) = Just (JBool False, s)
pValue ('n':'u':'l':'l':s)  = Just (JNull, s)
pValue (c:s) | c == '-' || isDigit' c = Just (pNum (c:s))
pValue _ = Nothing

pRawStr :: String -> Maybe (String, String)
pRawStr = go []
  where
    go acc ('"':s)    = Just (reverse acc, s)
    go acc ('\\':c:s) = go (c:'\\':acc) s
    go acc (c:s)      = go (c:acc) s
    go _   []         = Nothing

pObj :: String -> [(String, JValue)] -> Maybe (JValue, String)
pObj ('}':s) acc = Just (JObj (reverse acc), s)
pObj ('"':s) acc = do
  (key, s1) <- pRawStr s
  case skipWS s1 of
    ':':s2 -> do
      (val, s3) <- pValue (skipWS s2)
      case skipWS s3 of
        ',':s4 -> pObj (skipWS s4) ((key, val) : acc)
        '}':s4 -> Just (JObj (reverse ((key, val) : acc)), s4)
        _      -> Nothing
    _ -> Nothing
pObj _ _ = Nothing

pArr :: String -> [JValue] -> Maybe (JValue, String)
pArr (']':s) acc = Just (JArr (reverse acc), s)
pArr s acc = do
  (val, s1) <- pValue s
  case skipWS s1 of
    ',':s2 -> pArr (skipWS s2) (val : acc)
    ']':s2 -> Just (JArr (reverse (val : acc)), s2)
    _      -> Nothing

pNum :: String -> (JValue, String)
pNum s = let (n, rest) = span isNumChar s in (JNum n, rest)
  where isNumChar c = isDigit' c || c `elem` (".-+eE" :: String)

-- | Pretty-print a JValue with indentation.
renderJSON :: Int -> JValue -> String
renderJSON _ (JStr s)  = "\"" ++ s ++ "\""
renderJSON _ (JNum s)  = s
renderJSON _ (JBool b) = if b then "true" else "false"
renderJSON _ JNull     = "null"
renderJSON _ (JArr []) = "[]"
renderJSON n (JArr vs) =
  "[\n" ++ intercalate ",\n"
    (map (\v -> pad (n+2) ++ renderJSON (n+2) v) vs)
  ++ "\n" ++ pad n ++ "]"
renderJSON _ (JObj []) = "{}"
renderJSON n (JObj kvs) =
  "{\n" ++ intercalate ",\n"
    (map (\(k,v) -> pad (n+2) ++ "\"" ++ k ++ "\": " ++ renderJSON (n+2) v) kvs)
  ++ "\n" ++ pad n ++ "}"

pad :: Int -> String
pad n = replicate n ' '

isDigit' :: Char -> Bool
isDigit' c = c >= '0' && c <= '9'

isWS :: Char -> Bool
isWS c = c == ' ' || c == '\n' || c == '\r' || c == '\t'

skipWS :: String -> String
skipWS = dropWhile isWS

------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------

strip :: String -> String
strip = reverse . dropWhile isSpace' . reverse . dropWhile isSpace'
  where isSpace' c = c == ' ' || c == '\n' || c == '\r' || c == '\t'

lastLine :: String -> String
lastLine s = case filter (not . null) (lines s) of
  [] -> ""
  ls -> last ls

isWindows :: Bool
isWindows = "mingw" `isInfixOf` os || "windows" `isInfixOf` os

isMacOS :: Bool
isMacOS = os == "darwin"
