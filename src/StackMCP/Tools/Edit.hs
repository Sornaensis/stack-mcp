{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StackMCP.Tools.Edit
  ( tools
  , dispatch
  ) where

import Control.Exception (SomeException, try, catch)
import Data.Aeson.Key (fromText)
import Data.Char (isUpper, isAlphaNum)
import Data.IORef
import Data.List (nub, sort)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory
    ( doesDirectoryExist, doesFileExist, listDirectory
    , createDirectoryIfMissing, renameFile, removeDirectory )
import System.FilePath ((</>), takeExtension, takeDirectory, makeRelative, dropExtension)
import StackMCP.Tools.Common

tools :: [ToolDef]
tools =
  [ projectAddDepDef
  , projectRemoveDepDef
  , projectAddModuleDef
  , projectExposeModuleDef
  , projectRenameModuleDef
  , projectListModulesDef
  ]

dispatch :: IORef (Maybe FilePath) -> Text -> Value -> Maybe (IO ToolResult)
dispatch cwdRef name params = case name of
  "project_add_dependency"    -> Just $ withDir cwdRef (callAddDep params)
  "project_remove_dependency" -> Just $ withDir cwdRef (callRemoveDep params)
  "project_add_module"        -> Just $ withDir cwdRef (callAddModule params)
  "project_expose_module"     -> Just $ withDir cwdRef (callExposeModule params)
  "project_rename_module"     -> Just $ withDir cwdRef (callRenameModule params)
  "project_list_modules"      -> Just $ withDir cwdRef (callListModules params)
  _                           -> Nothing

-- | Read the cwdRef, fail if unset.  Catches exceptions from file I/O.
withDir :: IORef (Maybe FilePath) -> (FilePath -> IO ToolResult) -> IO ToolResult
withDir cwdRef action = do
  mcwd <- readIORef cwdRef
  case mcwd of
    Nothing  -> pure $ mkToolError "No project directory set. Call set_repo first."
    Just dir -> action dir `catch` (\(e :: SomeException) ->
      pure $ mkToolError ("Edit tool error: " <> T.pack (show e)))

------------------------------------------------------------------------
-- Definitions
------------------------------------------------------------------------

projectAddDepDef :: ToolDef
projectAddDepDef = ToolDef "project_add_dependency"
  "Add a package dependency to the project's package.yaml. \
  \Defaults to the top-level 'dependencies' list (shared by all components). \
  \Use section='library' to add a library-specific dependency. \
  \If already present, reports without duplicating." $
  mkSchema
    [ ("package", strProp "Package name (e.g. \"aeson\", \"text >= 2.0\").")
    , ("section", enumProp
        "Which section to add the dependency to. Defaults to top-level 'dependencies'."
        ["dependencies", "library"])
    ] ["package"]

projectRemoveDepDef :: ToolDef
projectRemoveDepDef = ToolDef "project_remove_dependency"
  "Remove a package dependency from the project's package.yaml. \
  \Defaults to removing from the top-level 'dependencies' list. \
  \Use section='library' to remove from the library-specific dependencies." $
  mkSchema
    [ ("package", strProp "Package name to remove (matched by prefix, e.g. \"aeson\" matches \"aeson >= 2.0\").")
    , ("section", enumProp
        "Which section to remove from. Defaults to top-level 'dependencies'."
        ["dependencies", "library"])
    ] ["package"]

projectAddModuleDef :: ToolDef
projectAddModuleDef = ToolDef "project_add_module"
  "Create a new Haskell module file in the project. \
  \Creates parent directories as needed. Generates a minimal module skeleton. \
  \Defaults to the library source directory. Use source_dir to target app/ or test/. \
  \Does NOT auto-add to exposed-modules (Hpack auto-discovers from source-dirs). \
  \Use project_expose_module if explicit exposed-modules are already configured." $
  mkSchema
    [ ("module_name", strProp "Fully qualified module name (e.g. \"Data.MyLib.Utils\").")
    , ("content", strProp "Optional module content. If omitted, generates a minimal skeleton.")
    , ("source_dir", strProp "Source directory to create the module in (e.g. \"app\", \"test\"). Defaults to library source-dirs.")
    ] ["module_name"]

projectExposeModuleDef :: ToolDef
projectExposeModuleDef = ToolDef "project_expose_module"
  "Add a module to the 'exposed-modules' list in the library section of package.yaml. \
  \Only needed if package.yaml already uses an explicit exposed-modules list. \
  \If no exposed-modules list exists yet, this creates one." $
  mkSchema
    [ ("module_name", strProp "Fully qualified module name (e.g. \"Data.MyLib.Utils\").")
    ] ["module_name"]

projectRenameModuleDef :: ToolDef
projectRenameModuleDef = ToolDef "project_rename_module"
  "Rename a Haskell module: moves the file, updates the module declaration inside it, \
  \and rewrites import statements across all .hs files in the project source directories. \
  \Covers library, app, and test source-dirs." $
  mkSchema
    [ ("old_name", strProp "Current fully qualified module name (e.g. \"Lib\").")
    , ("new_name", strProp "New fully qualified module name (e.g. \"MyLib.Core\").")
    ] ["old_name", "new_name"]

projectListModulesDef :: ToolDef
projectListModulesDef = ToolDef "project_list_modules"
  "List all Haskell modules found in the project's source directories (library, app, test). \
  \Returns module names grouped by source directory." $
  mkSchema
    [ ("section", enumProp
        "Which source dirs to scan. Defaults to all."
        ["all", "library", "executable", "test"])
    ] []

------------------------------------------------------------------------
-- package.yaml helpers (line-based, no YAML library needed)
------------------------------------------------------------------------

-- | Read package.yaml lines from a project directory.
readPackageYaml :: FilePath -> IO (Either Text [Text])
readPackageYaml dir = do
  let path = dir </> "package.yaml"
  exists <- doesFileExist path
  if not exists
    then pure $ Left "No package.yaml found in the project directory."
    else do
      content <- TIO.readFile path
      pure $ Right (T.lines content)

-- | Write package.yaml back.
writePackageYaml :: FilePath -> [Text] -> IO ()
writePackageYaml dir lns =
  TIO.writeFile (dir </> "package.yaml") (T.unlines lns)

-- | Find the line index of a top-level key like "dependencies:".
findTopLevelKey :: Text -> [Text] -> Maybe Int
findTopLevelKey key lns =
  let target = key <> ":"
  in case filter (\(_, l) -> T.stripEnd l == target || T.isPrefixOf (target <> " ") l) (zip [0..] lns) of
      ((i, _):_) -> Just i
      []         -> Nothing

-- | Find all "- item" entries after a given line index, at the expected indent.
--   Blank lines between entries are skipped.
findListItems :: Int -> [Text] -> [(Int, Text)]
findListItems startIdx lns =
  let afterKey = drop (startIdx + 1) (zip [0..] lns)
      -- Skip leading blank lines to find first list item
  in case dropWhile (\(_, l) -> T.null (T.strip l)) afterKey of
      ((_, firstLine):_)
        | "- " `T.isInfixOf` T.stripStart firstLine ->
            let indent = T.length (T.takeWhile (== ' ') firstLine)
            in collectItems indent afterKey
      _ -> []
  where
    collectItems _ [] = []
    collectItems indent ((i, l):rest)
      | T.null (T.strip l) = collectItems indent rest  -- skip blank lines
      | T.isPrefixOf "- " (T.stripStart l)
      , T.length (T.takeWhile (== ' ') l) >= indent = (i, l) : collectItems indent rest
      | otherwise = []

-- | Get the indent of the first list item under a key, or default to key indent + 2.
listIndent :: Int -> [Text] -> Int
listIndent keyIdx lns =
  case findListItems keyIdx lns of
    ((_, l):_) -> T.length (T.takeWhile (== ' ') l)
    []         -> T.length (T.takeWhile (== ' ') (lns !! keyIdx)) + 2

------------------------------------------------------------------------
-- Implementations
------------------------------------------------------------------------

-- | Add a dependency to package.yaml.
callAddDep :: Value -> FilePath -> IO ToolResult
callAddDep params dir = do
  let pkg     = T.strip $ getParamText "package" params
      section = T.strip $ getParamText "section" params
  if T.null pkg
    then pure $ mkToolError "package parameter is required"
    else do
      ePkgYaml <- readPackageYaml dir
      case ePkgYaml of
        Left err -> pure $ mkToolError err
        Right lns
          | section == "library" -> addDepToLibrary lns pkg dir
          | otherwise            -> addDepTopLevel lns pkg dir

-- | Add a dependency to the top-level 'dependencies' list.
addDepTopLevel :: [Text] -> Text -> FilePath -> IO ToolResult
addDepTopLevel lns pkg dir = do
  let pkgBase = T.takeWhile (\c -> c /= ' ' && c /= '>') pkg
  case findTopLevelKey "dependencies" lns of
    Nothing -> do
      -- No top-level dependencies key; append one
      let newLines = lns ++ ["", "dependencies:", "- " <> pkg]
      writePackageYaml dir newLines
      pure $ mkToolResultJSON $ object
        [ "success" .= True, "action" .= ("added" :: Text)
        , "package" .= pkg
        , "note" .= ("Created new dependencies section" :: Text)
        ]
    Just keyIdx -> do
      let items = findListItems keyIdx lns
          existing = map (\(_, l) -> T.strip (T.drop 2 (T.stripStart l))) items
          alreadyPresent = any (\e -> T.takeWhile (\c -> c /= ' ' && c /= '>') e == pkgBase) existing
      if alreadyPresent
        then pure $ mkToolResultJSON $ object
          [ "success" .= True, "action" .= ("already_present" :: Text)
          , "package" .= pkgBase
          ]
        else do
          let indent = listIndent keyIdx lns
              newLine = T.replicate indent " " <> "- " <> pkg
              insertAt = case items of
                [] -> keyIdx + 1
                _  -> fst (last items) + 1
              (before, after) = splitAt insertAt lns
              newLines = before ++ [newLine] ++ after
          writePackageYaml dir newLines
          pure $ mkToolResultJSON $ object
            [ "success" .= True, "action" .= ("added" :: Text)
            , "package" .= pkg
            ]

-- | Add a dependency to the library section's 'dependencies' sub-key.
addDepToLibrary :: [Text] -> Text -> FilePath -> IO ToolResult
addDepToLibrary lns pkg dir = do
  let pkgBase = T.takeWhile (\c -> c /= ' ' && c /= '>') pkg
  case findTopLevelKey "library" lns of
    Nothing -> pure $ mkToolError "No library section found in package.yaml"
    Just libIdx -> do
      let block = takeLibraryBlock libIdx lns
      case findSubKey "dependencies" libIdx block lns of
        Just depKeyIdx -> do
          let items = findListItems depKeyIdx lns
              existing = map (\(_, l) -> T.strip (T.drop 2 (T.stripStart l))) items
              alreadyPresent = any (\e -> T.takeWhile (\c -> c /= ' ' && c /= '>') e == pkgBase) existing
          if alreadyPresent
            then pure $ mkToolResultJSON $ object
              [ "success" .= True, "action" .= ("already_present" :: Text)
              , "package" .= pkgBase, "section" .= ("library" :: Text)
              ]
            else do
              let indent = listIndent depKeyIdx lns
                  newLine = T.replicate indent " " <> "- " <> pkg
                  insertAt = case items of
                    [] -> depKeyIdx + 1
                    _  -> fst (last items) + 1
                  (before, after) = splitAt insertAt lns
                  newLines = before ++ [newLine] ++ after
              writePackageYaml dir newLines
              pure $ mkToolResultJSON $ object
                [ "success" .= True, "action" .= ("added" :: Text)
                , "package" .= pkg, "section" .= ("library" :: Text)
                ]
        Nothing -> do
          -- Create dependencies sub-key under library
          let keyIndent = inferBlockIndent block
              itemIndent = keyIndent + 2
              insertAt = libIdx + 1
              newChunk = [ T.replicate keyIndent " " <> "dependencies:"
                         , T.replicate itemIndent " " <> "- " <> pkg
                         ]
              (before, after) = splitAt insertAt lns
              newLines = before ++ newChunk ++ after
          writePackageYaml dir newLines
          pure $ mkToolResultJSON $ object
            [ "success" .= True, "action" .= ("added" :: Text)
            , "package" .= pkg, "section" .= ("library" :: Text)
            , "note" .= ("Created new dependencies section under library" :: Text)
            ]

-- | Remove a dependency from package.yaml.
callRemoveDep :: Value -> FilePath -> IO ToolResult
callRemoveDep params dir = do
  let pkg     = T.strip $ getParamText "package" params
      section = T.strip $ getParamText "section" params
  if T.null pkg
    then pure $ mkToolError "package parameter is required"
    else do
      ePkgYaml <- readPackageYaml dir
      case ePkgYaml of
        Left err -> pure $ mkToolError err
        Right lns
          | section == "library" -> removeDepFromLibrary lns pkg dir
          | otherwise            -> removeDepTopLevel lns pkg dir

-- | Remove a dependency from the top-level 'dependencies' list.
removeDepTopLevel :: [Text] -> Text -> FilePath -> IO ToolResult
removeDepTopLevel lns pkg dir =
  case findTopLevelKey "dependencies" lns of
    Nothing -> pure $ mkToolError "No dependencies section found in package.yaml"
    Just keyIdx -> removeDepFromList lns keyIdx pkg dir Nothing

-- | Remove a dependency from the library section's 'dependencies' sub-key.
removeDepFromLibrary :: [Text] -> Text -> FilePath -> IO ToolResult
removeDepFromLibrary lns pkg dir =
  case findTopLevelKey "library" lns of
    Nothing -> pure $ mkToolError "No library section found in package.yaml"
    Just libIdx -> do
      let block = takeLibraryBlock libIdx lns
      case findSubKey "dependencies" libIdx block lns of
        Nothing -> pure $ mkToolError "No dependencies section found under library"
        Just depKeyIdx -> removeDepFromList lns depKeyIdx pkg dir (Just "library")

-- | Shared logic: remove a dep from a list at the given key index.
--   Normalises the user-supplied package name to the base name (before any
--   version constraint) so that e.g. "aeson >= 2.0" matches an "aeson" entry.
removeDepFromList :: [Text] -> Int -> Text -> FilePath -> Maybe Text -> IO ToolResult
removeDepFromList lns keyIdx pkg dir mSection = do
  let pkgBase = T.takeWhile (\c -> c /= ' ' && c /= '>') pkg
      items = findListItems keyIdx lns
      matchIdx = [i | (i, l) <- items,
                  let val = T.strip (T.drop 2 (T.stripStart l)),
                  T.takeWhile (\c -> c /= ' ' && c /= '>') val == pkgBase]
  case matchIdx of
    []    -> pure $ mkToolResultJSON $ object
      [ "success" .= False
      , "message" .= ("Package not found in dependencies: " <> pkgBase)
      ]
    (i:_) -> do
      let afterRemove = take i lns ++ drop (i + 1) lns
          -- If this was the last item, also remove the now-empty key line
          -- to avoid leaving a bare "dependencies:" that Hpack reads as null.
          remaining = findListItems keyIdx afterRemove
          newLines = if null remaining && length items == 1
                     then take keyIdx afterRemove ++ drop (keyIdx + 1) afterRemove
                     else afterRemove
      writePackageYaml dir newLines
      pure $ mkToolResultJSON $ object $
        [ "success" .= True, "action" .= ("removed" :: Text)
        , "package" .= pkg
        ] ++ maybe [] (\s -> ["section" .= s]) mSection

-- | Create a new module file.
callAddModule :: Value -> FilePath -> IO ToolResult
callAddModule params dir = do
  let modName   = T.strip $ getParamText "module_name" params
      content   = getParamText "content" params
      srcDirParam = T.strip $ getParamText "source_dir" params
  if T.null modName
    then pure $ mkToolError "module_name parameter is required"
    else if not (validModuleName modName)
    then pure $ mkToolError ("Invalid module name: " <> modName <> ". Must be dot-separated capitalized identifiers (e.g. Data.MyLib.Utils)")
    else do
      srcDir <- if T.null srcDirParam
                then guessSrcDir dir
                else pure (T.unpack srcDirParam)  -- will be created via createDirectoryIfMissing
      let relPath = moduleToPath modName
          fullPath = dir </> srcDir </> relPath
          parentDir = takeDirectory fullPath
      exists <- doesFileExist fullPath
      if exists
        then pure $ mkToolResultJSON $ object
          [ "success" .= False
          , "message" .= ("Module file already exists: " <> T.pack fullPath)
          ]
        else do
          createDirectoryIfMissing True parentDir
          let body = if T.null content
                then T.unlines
                  [ "module " <> modName <> " where"
                  , ""
                  ]
                else content
          TIO.writeFile fullPath body
          pure $ mkToolResultJSON $ object
            [ "success" .= True
            , "module" .= modName
            , "path" .= T.pack fullPath
            , "source_dir" .= T.pack srcDir
            ]

-- | Add a module to exposed-modules in package.yaml.
callExposeModule :: Value -> FilePath -> IO ToolResult
callExposeModule params dir = do
  let modName = T.strip $ getParamText "module_name" params
  if T.null modName
    then pure $ mkToolError "module_name parameter is required"
    else do
      ePkgYaml <- readPackageYaml dir
      case ePkgYaml of
        Left err -> pure $ mkToolError err
        Right lns -> do
          -- Find "library:" section, then "exposed-modules:" within it
          case findTopLevelKey "library" lns of
            Nothing -> pure $ mkToolError "No library section found in package.yaml"
            Just libIdx -> do
              let libBlock = takeLibraryBlock libIdx lns
                  emIdx = findSubKey "exposed-modules" libIdx libBlock lns
              case emIdx of
                Just emKeyIdx -> do
                  let items = findListItems emKeyIdx lns
                      existing = map (\(_, l) -> T.strip (T.drop 2 (T.stripStart l))) items
                  if modName `elem` existing
                    then pure $ mkToolResultJSON $ object
                      [ "success" .= True, "action" .= ("already_exposed" :: Text)
                      , "module" .= modName
                      ]
                    else do
                      let indent = listIndent emKeyIdx lns
                          newLine = T.replicate indent " " <> "- " <> modName
                          insertAt = case items of
                            [] -> emKeyIdx + 1
                            _  -> fst (last items) + 1
                          (before, after) = splitAt insertAt lns
                          newLines = before ++ [newLine] ++ after
                      writePackageYaml dir newLines
                      pure $ mkToolResultJSON $ object
                        [ "success" .= True, "action" .= ("exposed" :: Text)
                        , "module" .= modName
                        ]
                Nothing -> do
                  -- No exposed-modules yet; insert one after library:
                  let insertAt = libIdx + 1
                      indent = inferBlockIndent libBlock
                      itemIndent = indent + 2  -- list items indented under key
                      newChunk = [ T.replicate indent " " <> "exposed-modules:"
                                 , T.replicate itemIndent " " <> "- " <> modName
                                 ]
                      (before, after) = splitAt insertAt lns
                      newLines = before ++ newChunk ++ after
                  writePackageYaml dir newLines
                  pure $ mkToolResultJSON $ object
                    [ "success" .= True, "action" .= ("created_and_exposed" :: Text)
                    , "module" .= modName
                    ]

-- | Rename a module: move file, update module header, rewrite imports.
callRenameModule :: Value -> FilePath -> IO ToolResult
callRenameModule params dir = do
  let oldName = T.strip $ getParamText "old_name" params
      newName = T.strip $ getParamText "new_name" params
  if T.null oldName || T.null newName
    then pure $ mkToolError "old_name and new_name parameters are required"
    else if not (validModuleName oldName) || not (validModuleName newName)
    then pure $ mkToolError "Module names must be dot-separated capitalized identifiers"
    else do
      srcDirs <- allSrcDirs dir
      -- Find the old module file
      let oldRelPath = moduleToPath oldName
      found <- findModuleIn dir srcDirs oldRelPath
      case found of
        Nothing -> pure $ mkToolError ("Module not found: " <> oldName)
        Just (srcDir, oldFullPath) -> do
          let newRelPath = moduleToPath newName
              newFullPath = dir </> srcDir </> newRelPath
              newParent = takeDirectory newFullPath
          newExists <- doesFileExist newFullPath
          if newExists
            then pure $ mkToolError ("Target module already exists: " <> T.pack newFullPath)
            else do
              -- Move the file
              createDirectoryIfMissing True newParent
              renameFile oldFullPath newFullPath
              -- Clean up empty parent directories left behind
              let srcRoot = dir </> srcDir
              cleanEmptyDirs srcRoot (takeDirectory oldFullPath)
              -- Update module declaration in the file
              content <- TIO.readFile newFullPath
              let updatedContent = rewriteModuleHeader oldName newName content
              TIO.writeFile newFullPath updatedContent
              -- Rewrite imports across all .hs files in all source dirs
              allHs <- concatMapM (findHsFiles dir) srcDirs
              updatedCount <- rewriteImports oldName newName allHs
              -- Update exposed-modules in package.yaml if old module was listed
              exposedUpdated <- updateExposedModules dir oldName newName
              pure $ mkToolResultJSON $ object
                [ "success" .= True
                , "old_module" .= oldName
                , "new_module" .= newName
                , "old_path" .= T.pack oldFullPath
                , "new_path" .= T.pack newFullPath
                , "imports_updated" .= updatedCount
                , "exposed_modules_updated" .= exposedUpdated
                ]

-- | List all modules in project source directories.
callListModules :: Value -> FilePath -> IO ToolResult
callListModules params dir = do
  let section = getParamText "section" params
  srcDirs <- allSrcDirs dir
  libSrcDir <- guessSrcDir dir
  let filter' = case section of
        "library"    -> \(s, _) -> s == libSrcDir
        "executable" -> \(s, _) -> s == "app" || s == "exe"
        "test"       -> \(s, _) -> s == "test" || s == "test-integration" || s == "tests"
        _            -> const True  -- "all" or empty
  results <- mapM (\sd -> do
      hsFiles <- findHsFiles dir sd
      let mods = sort $ map (pathToModule (dir </> sd)) hsFiles
      pure (sd, mods)
    ) srcDirs
  let filtered = filter filter' results
  pure $ mkToolResultJSON $ object
    [ "source_dirs" .= object
        [ fromText (T.pack sd) .= mods | (sd, mods) <- filtered ]
    ]

------------------------------------------------------------------------
-- Module name / path utilities
------------------------------------------------------------------------

-- | Validate a module name: dot-separated, each part starts with uppercase.
validModuleName :: Text -> Bool
validModuleName name =
  let parts = T.splitOn "." name
  in not (null parts)
     && all (\p -> not (T.null p) && isUpper (T.head p) && T.all (\c -> isAlphaNum c || c == '_' || c == '\'') p) parts

-- | Convert "Data.MyLib.Utils" to "Data/MyLib/Utils.hs".
moduleToPath :: Text -> FilePath
moduleToPath modName =
  T.unpack (T.replace "." "/" modName) ++ ".hs"

-- | Convert a file path relative to a source dir to a module name.
pathToModule :: FilePath -> FilePath -> Text
pathToModule srcDir fullPath =
  let rel = makeRelative srcDir fullPath
      noExt = dropExtension rel
  in T.replace "/" "." (T.replace "\\" "." (T.pack noExt))

-- | Guess the library source directory from package.yaml, defaulting to "src".
guessSrcDir :: FilePath -> IO FilePath
guessSrcDir dir = do
  ePkgYaml <- readPackageYaml dir
  case ePkgYaml of
    Right lns -> do
      case findTopLevelKey "library" lns of
        Just libIdx -> do
          let block = takeLibraryBlock libIdx lns
          case findSourceDirsValue libIdx block lns of
            Just sd -> pure (T.unpack sd)
            Nothing -> pure "src"
        Nothing -> pure "src"
    Left _ -> pure "src"

-- | Get all source dirs from package.yaml (library, executables, tests).
allSrcDirs :: FilePath -> IO [FilePath]
allSrcDirs dir = do
  ePkgYaml <- readPackageYaml dir
  case ePkgYaml of
    Right lns -> do
      let dirs = nub $ extractAllSourceDirs lns
      existing <- filterM (\d -> doesDirectoryExist (dir </> d)) dirs
      pure $ if null existing then ["src"] else existing
    Left _ -> do
      srcExists <- doesDirectoryExist (dir </> "src")
      pure $ if srcExists then ["src"] else ["."]
  where
    filterM _ [] = pure []
    filterM p (x:xs) = do
      b <- p x
      rest <- filterM p xs
      pure $ if b then x : rest else rest

-- | Extract all source-dirs values from package.yaml lines.
--   Handles both inline (source-dirs: src) and multi-line (source-dirs:\n- src\n- lib) forms.
--   Uses prefix match on stripped line to avoid matching comments or unrelated keys.
extractAllSourceDirs :: [Text] -> [FilePath]
extractAllSourceDirs lns = concatMap extractFromLine (zip [(0::Int)..] lns)
  where
    extractFromLine (i, l)
      | T.isPrefixOf "source-dirs:" (T.stripStart l) =
          let after = T.strip (T.drop 1 (snd (T.breakOn ":" l)))
          in if T.null after
             then -- Multi-line form: collect "- item" entries below
                  map (\(_, item) -> T.unpack (T.strip (T.drop 2 (T.stripStart item))))
                      (findListItems i lns)
             else [T.unpack after]
      | otherwise = []

-- | Find .hs files recursively under a source dir.
findHsFiles :: FilePath -> FilePath -> IO [FilePath]
findHsFiles projectDir srcDir = do
  let fullSrcDir = projectDir </> srcDir
  exists <- doesDirectoryExist fullSrcDir
  if not exists then pure []
  else go fullSrcDir
  where
    go d = do
      entries <- listDirectory d `catch` (\(_ :: SomeException) -> pure [])
      fmap concat $ mapM (\e -> do
          let full = d </> e
          isDir <- doesDirectoryExist full
          if isDir
            then go full
            else pure [full | takeExtension e == ".hs"]
        ) entries

-- | Find a module file in one of the source dirs.
findModuleIn :: FilePath -> [FilePath] -> FilePath -> IO (Maybe (FilePath, FilePath))
findModuleIn projectDir srcDirs relPath = go srcDirs
  where
    go [] = pure Nothing
    go (sd : rest) = do
      let full = projectDir </> sd </> relPath
      exists <- doesFileExist full
      if exists then pure (Just (sd, full))
      else go rest

-- | Rewrite "module OldName" to "module NewName" in file content.
--   Uses word-boundary-aware matching to avoid corrupting substrings.
rewriteModuleHeader :: Text -> Text -> Text -> Text
rewriteModuleHeader oldName newName content =
  let lns = T.lines content
      updated = map (\l ->
        if T.isPrefixOf "module " (T.stripStart l)
        then let ws = T.stripStart l
                 afterMod = T.stripStart (T.drop 7 ws)  -- "module " is 7 chars
                 modToken = T.takeWhile isModChar afterMod
             in if modToken == oldName
                then replaceFirst oldName newName l
                else l
        else l
        ) lns
  in T.unlines updated

-- | Rewrite "import OldName" and "import qualified OldName" across all files.
--   Returns count of files that were modified.
rewriteImports :: Text -> Text -> [FilePath] -> IO Int
rewriteImports oldName newName files = do
  counts <- mapM rewriteOne files
  pure (sum counts)
  where
    rewriteOne :: FilePath -> IO Int
    rewriteOne path = do
      result <- try (TIO.readFile path) :: IO (Either SomeException Text)
      case result of
        Left _ -> pure 0
        Right content -> do
          let lns = T.lines content
              (updated, changed) = foldr (\l (acc, c) ->
                let l' = rewriteImportLine oldName newName l
                in (l' : acc, c || l' /= l)
                ) ([], False) lns
          if changed
            then do
              TIO.writeFile path (T.unlines updated)
              pure 1
            else pure 0

-- | Rewrite a single import line if it references the old module.
--   Parses the import syntax to extract the exact module name token,
--   avoiding substring corruption (e.g. renaming "Lib" won't touch "Data.Library").
--   Handles: import X, import qualified X, import X qualified,
--            import {-# SOURCE #-} X, import {-# SOURCE #-} qualified X,
--            import "pkg" X (PackageImports extension)
rewriteImportLine :: Text -> Text -> Text -> Text
rewriteImportLine oldName newName line
  | not (T.isPrefixOf "import" (T.stripStart line)) = line
  | otherwise =
      let ws = T.stripStart line
          afterImport = T.stripStart (T.drop 6 ws)  -- "import" is 6 chars
          -- Skip optional {-# ... #-} pragma (e.g. SOURCE)
          afterSource
            | T.isPrefixOf "{-#" afterImport =
                let (_, rest) = T.breakOn "#-}" afterImport
                in if T.null rest
                   then afterImport  -- malformed pragma, skip
                   else T.stripStart (T.drop 3 rest)  -- skip "#-}"
            | otherwise = afterImport
          -- Skip optional "qualified" keyword
          afterQual
            | T.isPrefixOf "qualified " afterSource = T.stripStart (T.drop 9 afterSource)
            | otherwise = afterSource
          -- Skip optional PackageImports string (e.g. "base")
          afterPkgImport
            | not (T.null afterQual) && T.head afterQual == '"' =
                let rest = T.drop 1 afterQual  -- skip opening quote
                    (_, afterClose) = T.breakOn "\"" rest
                in if T.null afterClose
                   then afterQual  -- malformed, skip
                   else T.stripStart (T.drop 1 afterClose)  -- skip closing quote
            | otherwise = afterQual
          -- Extract the module name token
          modToken = T.takeWhile isModChar afterPkgImport
      in if modToken == oldName
         then replaceFirst oldName newName line
         else line

-- | Character that can appear in a Haskell module name.
isModChar :: Char -> Bool
isModChar c = isAlphaNum c || c == '.' || c == '_' || c == '\''

-- | Replace the first occurrence of a needle in a haystack.
replaceFirst :: Text -> Text -> Text -> Text
replaceFirst needle replacement haystack =
  let (before, rest) = T.breakOn needle haystack
  in if T.null rest then haystack
     else before <> replacement <> T.drop (T.length needle) rest

-- | Take lines belonging to the library block (until next top-level key).
--   Continues past blank lines and column-0 comments ("# ...").
takeLibraryBlock :: Int -> [Text] -> [(Int, Text)]
takeLibraryBlock keyIdx lns =
  let afterKey = drop (keyIdx + 1) (zip [0..] lns)
  in takeWhile (\(_, l) ->
       T.null (T.strip l) || T.isPrefixOf " " l || T.isPrefixOf "#" l
     ) afterKey

-- | Find a sub-key (like "exposed-modules") within a library block.
--   Uses prefix match on stripped line to avoid false positives from
--   keys that contain the target as a substring.
findSubKey :: Text -> Int -> [(Int, Text)] -> [Text] -> Maybe Int
findSubKey key _libIdx block _lns =
  let target = key <> ":"
  in case filter (\(_, l) -> T.isPrefixOf target (T.stripStart l)) block of
      ((i, _):_) -> Just i
      []         -> Nothing

-- | Infer the indent used by existing sub-keys in a block.
--   Falls back to 2 if the block has no indented content lines.
inferBlockIndent :: [(Int, Text)] -> Int
inferBlockIndent block =
  case [T.length (T.takeWhile (== ' ') l) | (_, l) <- block,
        not (T.null (T.strip l)), T.isPrefixOf " " l, not (T.isPrefixOf "#" l)] of
    (n:_) -> n
    []    -> 2

-- | Find source-dirs value in a library block.
--   Handles both inline (source-dirs: src) and multi-line block-sequence forms.
findSourceDirsValue :: Int -> [(Int, Text)] -> [Text] -> Maybe Text
findSourceDirsValue _libIdx block lns =
  case filter (\(_, l) -> T.isPrefixOf "source-dirs:" (T.stripStart l)) block of
    ((i, l):_) ->
      let after = T.strip (T.drop 1 (snd (T.breakOn ":" l)))
      in if T.null after
         then -- Multi-line form: return first list item
              case findListItems i lns of
                ((_, item):_) -> Just (T.strip (T.drop 2 (T.stripStart item)))
                []            -> Nothing
         else Just after
    [] -> Nothing

-- | Update exposed-modules in package.yaml: replace oldName with newName.
--   Returns True if the exposed-modules list was updated.
updateExposedModules :: FilePath -> Text -> Text -> IO Bool
updateExposedModules dir oldName newName = do
  ePkgYaml <- readPackageYaml dir
  case ePkgYaml of
    Right pkgLns ->
      case findTopLevelKey "library" pkgLns of
        Just libIdx -> do
          let block = takeLibraryBlock libIdx pkgLns
          case findSubKey "exposed-modules" libIdx block pkgLns of
            Just emIdx -> do
              let items = findListItems emIdx pkgLns
                  matches = [(i, l) | (i, l) <- items,
                             T.strip (T.drop 2 (T.stripStart l)) == oldName]
              case matches of
                ((matchIdx, matchLine):_) -> do
                  let newLine = replaceFirst oldName newName matchLine
                      updatedPkg = take matchIdx pkgLns ++ [newLine] ++ drop (matchIdx + 1) pkgLns
                  writePackageYaml dir updatedPkg
                  pure True
                [] -> pure False
            Nothing -> pure False
        Nothing -> pure False
    Left _ -> pure False

-- | concatMap for monadic actions.
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = fmap concat (mapM f xs)

-- | Remove empty parent directories between the given dir and the srcRoot.
--   Walks upward from dir, removing each directory if it's empty,
--   stopping at or before srcRoot.  Uses a length guard in addition to
--   equality to prevent runaway recursion from path normalisation mismatches.
cleanEmptyDirs :: FilePath -> FilePath -> IO ()
cleanEmptyDirs srcRoot dir
  | dir == srcRoot          = pure ()
  | length dir <= length srcRoot = pure ()
  | otherwise = do
      exists <- doesDirectoryExist dir
      if not exists then pure ()
      else do
        entries <- listDirectory dir `catch` (\(_ :: SomeException) -> pure ["x"])
        if null entries
          then do
            removeDirectory dir `catch` (\(_ :: SomeException) -> pure ())
            cleanEmptyDirs srcRoot (takeDirectory dir)
          else pure ()
