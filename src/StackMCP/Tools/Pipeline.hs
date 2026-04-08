{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StackMCP.Tools.Pipeline
  ( tools
  , dispatch
  ) where

import Control.Exception (SomeException, try, catch)
import Data.IORef
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (getCurrentDirectory, listDirectory, doesFileExist, canonicalizePath)
import System.FilePath ((</>), takeExtension, normalise, isRelative, pathSeparator, splitDirectories, dropTrailingPathSeparator)
import StackMCP.Tools.Common
import StackMCP.Tools.Parse (parseGhcDiagnostics, filteredDiagnosticsSummary, parseDepErrors, depErrorsSummary)

tools :: [ToolDef]
tools =
  [ stackPipelineDef
  , stackConfigReadDef
  ]

dispatch :: IORef (Maybe FilePath) -> Text -> Value -> Maybe (IO ToolResult)
dispatch cwdRef name params = case name of
  "stack_pipeline"    -> Just $ callPipeline cwdRef params
  "stack_config_read" -> Just $ callConfigRead cwdRef params
  _                   -> Nothing

------------------------------------------------------------------------
-- Definitions
------------------------------------------------------------------------

stackPipelineDef :: ToolDef
stackPipelineDef = ToolDef "stack_pipeline"
  "Execute a pipeline of Stack commands in sequence. Stops on first failure. \
  \Returns structured JSON with results for each step." $
  mkSchema
    [ ("steps", arrayProp "Array of stack subcommand strings to execute in order (e.g. [\"build\", \"test\", \"haddock\"]).")
    , ("include_warnings", boolProp "Include GHC warnings in diagnostics (default: false).")
    , ("include_output", boolProp "Include raw stdout/stderr per step (default: false).")
    ] ["steps"]

stackConfigReadDef :: ToolDef
stackConfigReadDef = ToolDef "stack_config_read"
  "Read and return the contents of project config files as text. \
  \Supports well-known files (stack.yaml, package.yaml, cabal, stack.yaml.lock) \
  \or an arbitrary relative path within the project directory." $
  mkSchema
    [ ("file", strProp "Well-known config key (stack.yaml, package.yaml, cabal, stack.yaml.lock) or a relative path to a file in the project directory (e.g. 'cabal.project', 'hie.yaml').")
    ] ["file"]

------------------------------------------------------------------------
-- Implementations
------------------------------------------------------------------------

callPipeline :: IORef (Maybe FilePath) -> Value -> IO ToolResult
callPipeline cwdRef params = do
  mcwd <- readIORef cwdRef
  let stepsRaw = getParamArray "steps" params
      inclW    = getParamBool "include_warnings" params
      inclO    = getParamBool "include_output" params
  if null stepsRaw
    then pure $ mkToolError "steps parameter is required and must be a non-empty array"
    else do
      results <- runPipeline mcwd inclW inclO stepsRaw 1 []
      let allSuccess = length results == length stepsRaw
      pure $ mkToolResultJSON $ object
        [ "success"         .= allSuccess
        , "steps_completed" .= length results
        , "steps_total"     .= length stepsRaw
        , "results"         .= results
        ]
  where
    runPipeline :: Maybe FilePath -> Bool -> Bool -> [Text] -> Int -> [Value] -> IO [Value]
    runPipeline _ _ _ [] _ acc = pure (reverse acc)
    runPipeline mcwd' inclW inclO (step:rest) n acc = do
      let args = T.words step
      so <- runStackBuild mcwd' args
      let success = soExitCode so == 0
          diags   = parseGhcDiagnostics (soStderr so)
          depErrs = parseDepErrors (soStderr so)
          mDiagSummary = filteredDiagnosticsSummary inclW diags
          result  = object $
            [ "step"    .= n
            , "command" .= ("stack " <> step)
            , "success" .= success
            ] ++ maybe [] (\d -> ["diagnostics" .= d]) mDiagSummary
              ++ ["dependency_errors" .= depErrorsSummary depErrs | not (null depErrs)]
              ++ ["output" .= soStdout so | inclO]
              ++ ["stderr" .= soStderr so | inclO || (not success && null diags && null depErrs)]
              ++ ["exit_code" .= soExitCode so | not success]
      if success
        then runPipeline mcwd' inclW inclO rest (n + 1) (result : acc)
        else pure (reverse (result : acc))

callConfigRead :: IORef (Maybe FilePath) -> Value -> IO ToolResult
callConfigRead cwdRef params = do
  mcwd <- readIORef cwdRef
  dir <- case mcwd of
    Just d  -> pure d
    Nothing -> getCurrentDirectory
  let fileKey = getParamText "file" params
  case fileKey of
    "stack.yaml"      -> readConfigFile dir "stack.yaml"
    "package.yaml"    -> readConfigFile dir "package.yaml"
    "stack.yaml.lock" -> readConfigFile dir "stack.yaml.lock"
    "cabal" -> do
      files <- listDirectory dir `catch` (\(_ :: SomeException) -> pure [])
      let cabalFiles = filter (\f -> takeExtension f == ".cabal") files
      case cabalFiles of
        (f:_) -> readConfigFile dir f
        []    -> pure $ mkToolError "No .cabal file found in project directory"
    _ -> readArbitraryFile dir (T.unpack fileKey)
  where
    readConfigFile :: FilePath -> FilePath -> IO ToolResult
    readConfigFile dir' filename = do
      let path = dir' </> filename
      result <- try (TIO.readFile path) :: IO (Either SomeException Text)
      case result of
        Right content -> pure $ mkToolResultJSON $ object
          [ "file"    .= T.pack filename
          , "path"    .= T.pack path
          , "content" .= content
          ]
        Left err -> pure $ mkToolError ("File not found or unreadable: " <> T.pack path
                                        <> " (" <> T.pack (show err) <> ")")

    readArbitraryFile :: FilePath -> FilePath -> IO ToolResult
    readArbitraryFile dir' filename
      | not (isRelative filename) = pure $ mkToolError "Path must be relative to the project directory"
      | ".." `elem` map dropTrailingPathSeparator (splitDirectories filename) = pure $ mkToolError "Path traversal ('..') is not allowed"
      | otherwise = do
          let raw = dir' </> normalise filename
          canonical <- try (canonicalizePath raw) :: IO (Either SomeException FilePath)
          canonDir  <- canonicalizePath dir'
          case canonical of
            Left _ -> pure $ mkToolError ("File not found: " <> T.pack filename)
            Right resolved
              | not (canonDir `isParentOf` resolved) ->
                  pure $ mkToolError "Path must be within the project directory"
              | otherwise -> do
                  exists <- doesFileExist resolved
                  if exists
                    then do
                      result <- try (TIO.readFile resolved) :: IO (Either SomeException Text)
                      case result of
                        Right content -> pure $ mkToolResultJSON $ object
                          [ "file"    .= T.pack filename
                          , "path"    .= T.pack resolved
                          , "content" .= content
                          ]
                        Left err -> pure $ mkToolError ("File not found or unreadable: " <> T.pack resolved
                                                        <> " (" <> T.pack (show err) <> ")")
                    else pure $ mkToolError ("File not found: " <> T.pack filename)

    isParentOf parent child =
      let p = addSep (normalise parent)
      in p `T.isPrefixOf` T.pack (normalise child) || normalise parent == normalise child
      where
        addSep s
          | null s         = T.empty
          | last s == '/'  = T.pack s
          | last s == '\\' = T.pack s
          | otherwise      = T.pack s <> T.singleton pathSeparator


