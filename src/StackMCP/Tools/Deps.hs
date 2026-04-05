{-# LANGUAGE OverloadedStrings #-}

module StackMCP.Tools.Deps
  ( tools
  , dispatch
  ) where

import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import StackMCP.Tools.Common

tools :: [ToolDef]
tools =
  [ stackLsDepsDef
  , stackLsDepsJsonDef
  , stackLsDepsTreeDef
  , stackLsSnapshotsDef
  , stackLsGlobalsDef
  , stackUnpackDef
  , stackUpdateDef
  , stackListDef
  , stackDotDef
  , stackSdistDef
  , stackUploadDef
  ]

dispatch :: Maybe FilePath -> Text -> Value -> Maybe (IO ToolResult)
dispatch mcwd name params = case name of
  "stack_ls_dependencies"      -> Just $ callStackLsDeps mcwd params
  "stack_ls_dependencies_json" -> Just $ callStackLsDepsJson mcwd params
  "stack_ls_dependencies_tree" -> Just $ callStackLsDepsTree mcwd params
  "stack_ls_snapshots"         -> Just $ callStackLsSnapshots mcwd params
  "stack_ls_globals"           -> Just $ callStackLsGlobals mcwd
  "stack_unpack"               -> Just $ callStackUnpack mcwd params
  "stack_update"               -> Just $ callStackUpdate mcwd
  "stack_list"                 -> Just $ callStackList mcwd params
  "stack_dot"                  -> Just $ callStackDot mcwd params
  "stack_sdist"                -> Just $ callStackSdist mcwd params
  "stack_upload"               -> Just $ callStackUpload mcwd params
  _                            -> Nothing

------------------------------------------------------------------------
-- Definitions
------------------------------------------------------------------------

stackLsDepsDef :: ToolDef
stackLsDepsDef = ToolDef "stack_ls_dependencies"
  "List project dependencies as text (name version per line). \
  \Returns parsed name/version pairs. For raw JSON from Stack, use stack_ls_dependencies_json. \
  \For a visual tree, use stack_ls_dependencies_tree." $
  mkSchema
    [ ("depth", intProp "Maximum depth of dependency tree.")
    , ("license", boolProp "Print license instead of version (--license).")
    , ("external", boolProp "Include only external dependencies (--external).")
    , ("separator", strProp "Separator between package name and version (default: space).")
    , ("filter", strProp "Package name or $locals to filter out (can repeat, comma-separated).")
    , ("prune", strProp "Comma-separated packages to prune from the tree.")
    , ("test", boolProp "Consider dependencies of test components (--test).")
    , ("bench", boolProp "Consider dependencies of benchmark components (--bench).")
    ] []

stackLsDepsJsonDef :: ToolDef
stackLsDepsJsonDef = ToolDef "stack_ls_dependencies_json"
  "List project dependencies as structured JSON (uses Stack's native JSON output). \
  \Best for programmatic analysis. For a human-readable list, use stack_ls_dependencies." $
  mkSchema
    [ ("depth", intProp "Maximum depth of dependency tree.")
    , ("external", boolProp "Include only external dependencies (--external).")
    , ("prune", strProp "Comma-separated packages to prune.")
    , ("test", boolProp "Consider test dependencies (--test).")
    , ("bench", boolProp "Consider benchmark dependencies (--bench).")
    ] []

stackLsDepsTreeDef :: ToolDef
stackLsDepsTreeDef = ToolDef "stack_ls_dependencies_tree"
  "List project dependencies as an indented tree (visual format). \
  \Best for understanding dependency hierarchy. For structured data, use stack_ls_dependencies_json." $
  mkSchema
    [ ("depth", intProp "Maximum depth.")
    , ("external", boolProp "Include only external dependencies (--external).")
    , ("license", boolProp "Print license instead of version (--license).")
    , ("prune", strProp "Comma-separated packages to prune.")
    , ("test", boolProp "Consider test dependencies (--test).")
    , ("bench", boolProp "Consider benchmark dependencies (--bench).")
    ] []

stackLsSnapshotsDef :: ToolDef
stackLsSnapshotsDef = ToolDef "stack_ls_snapshots"
  "List available snapshots (LTS or Nightly)." $
  mkSchema
    [ ("snapshot_type", enumProp "Filter by snapshot type." ["lts", "nightly"])
    , ("source", enumProp "Snapshot source." ["local", "remote"])
    ] []

stackLsGlobalsDef :: ToolDef
stackLsGlobalsDef = ToolDef "stack_ls_globals"
  "List global packages in the active GHC environment." $
  mkSchema [] []

stackUnpackDef :: ToolDef
stackUnpackDef = ToolDef "stack_unpack"
  "Unpack one or more packages from Hackage locally." $
  mkSchema
    [ ("package", strProp "Package name, optionally with version (e.g. aeson-2.2.3.0).")
    ] ["package"]

stackUpdateDef :: ToolDef
stackUpdateDef = ToolDef "stack_update"
  "Update the Hackage package index." $
  mkSchema [] []

stackListDef :: ToolDef
stackListDef = ToolDef "stack_list"
  "List package versions from the package index or a snapshot." $
  mkSchema
    [ ("package", strProp "Package name pattern to search for.")
    , ("snapshot", strProp "Snapshot to list from (e.g. lts-24.2).")
    ] []

stackDotDef :: ToolDef
stackDotDef = ToolDef "stack_dot"
  "Output dependency graph in Graphviz DOT format." $
  mkSchema
    [ ("targets", strProp "Space-separated targets.")
    , ("external", boolProp "Include external dependencies (--external).")
    , ("depth", intProp "Maximum depth.")
    , ("prune", strProp "Comma-separated packages to prune.")
    , ("flags", strProp "Additional raw flags.")
    ] []

stackSdistDef :: ToolDef
stackSdistDef = ToolDef "stack_sdist"
  "Create source distribution tarballs." $
  mkSchema
    [ ("targets", strProp "Space-separated targets.")
    , ("flags", strProp "Additional raw flags.")
    ] []

stackUploadDef :: ToolDef
stackUploadDef = ToolDef "stack_upload"
  "Upload packages or documentation to Hackage." $
  mkSchema
    [ ("targets", strProp "Space-separated targets or tarball paths.")
    , ("documentation", boolProp "Upload documentation instead of package (--documentation).")
    , ("candidate", boolProp "Upload as a package candidate (--candidate).")
    , ("flags", strProp "Additional raw flags.")
    ] []

------------------------------------------------------------------------
-- Implementations
------------------------------------------------------------------------

-- | Common args for ls dependencies subcommand.
depsCommon :: Value -> [Text]
depsCommon params =
  let depth    = getParamInt "depth" params
      external = getParamBool "external" params
      prune    = getParamText "prune" params
      test     = getParamBool "test" params
      bench    = getParamBool "bench" params
      depthArg = case depth of
        Just n  -> ["--depth", T.pack (show n)]
        Nothing -> []
  in  depthArg
   ++ ["--external" | external]
   ++ ["--prune" | not (T.null prune)] ++ [prune | not (T.null prune)]
   ++ ["--test" | test]
   ++ ["--bench" | bench]

callStackLsDeps :: Maybe FilePath -> Value -> IO ToolResult
callStackLsDeps mcwd params = do
  let license  = getParamBool "license" params
      sep      = getParamText "separator" params
      filt     = getParamText "filter" params
      args = ["ls", "dependencies", "text"]
          ++ depsCommon params
          ++ ["--license" | license]
          ++ ["--separator" | not (T.null sep)] ++ [sep | not (T.null sep)]
          ++ concatMap (\f -> ["--filter", f]) (splitComma filt)
  so <- runStackBuild mcwd args
  case soExitCode so of
    0 -> do
      let ls = filter (not . T.null) $ T.lines (soStdout so)
          parsed = [ let ws = T.words l in case ws of
                       (n:v:_) -> object ["name" .= n, "version" .= v]
                       [n]     -> object ["name" .= n, "version" .= ("" :: Text)]
                       _       -> object ["name" .= l, "version" .= ("" :: Text)]
                   | l <- ls
                   ]
      pure $ mkToolResultJSON $ object
        [ "count"        .= length parsed
        , "dependencies" .= parsed
        ]
    _ -> pure $ mkCommandError args so

callStackLsDepsJson :: Maybe FilePath -> Value -> IO ToolResult
callStackLsDepsJson mcwd params = do
  let args = ["ls", "dependencies", "json"] ++ depsCommon params
  so <- runStackBuild mcwd args
  case soExitCode so of
    0 -> case eitherDecodeStrict (TE.encodeUtf8 (soStdout so)) of
           Right v  -> pure $ mkToolResultJSON v
           Left err -> pure $ mkToolErrorJSON $ object
             [ "error_type" .= ("json_parse_error" :: Text)
             , "message"    .= T.pack err
             , "raw_output" .= soStdout so
             ]
    _ -> pure $ mkCommandError args so

callStackLsDepsTree :: Maybe FilePath -> Value -> IO ToolResult
callStackLsDepsTree mcwd params = do
  let license  = getParamBool "license" params
      args = ["ls", "dependencies", "tree"]
          ++ depsCommon params
          ++ ["--license" | license]
  so <- runStackBuild mcwd args
  pure $ case soExitCode so of
    0 -> mkToolResultJSON $ object ["tree" .= soStdout so]
    _ -> mkCommandError args so

callStackLsSnapshots :: Maybe FilePath -> Value -> IO ToolResult
callStackLsSnapshots mcwd params = do
  let ty     = getParamText "snapshot_type" params
      source = getParamText "source" params
      args = ["ls", "snapshots"]
          ++ [source | not (T.null source)]
          ++ ["--lts" | ty == "lts"]
          ++ ["--nightly" | ty == "nightly"]
  so <- runStackBuild mcwd args
  pure $ case soExitCode so of
    0 -> mkToolResultJSON $ object ["output" .= soStdout so]
    _ -> mkCommandError args so

callStackLsGlobals :: Maybe FilePath -> IO ToolResult
callStackLsGlobals mcwd = do
  so <- runStackBuild mcwd ["ls", "globals"]
  case soExitCode so of
    0 -> do
      let ls = filter (not . T.null) $ T.lines (soStdout so)
          parsed = [ let ws = T.words l in case ws of
                       (n:v:_) -> object ["name" .= n, "version" .= v]
                       [n]     -> object ["name" .= n, "version" .= ("" :: Text)]
                       _       -> object ["name" .= l, "version" .= ("" :: Text)]
                   | l <- ls
                   ]
      pure $ mkToolResultJSON $ object ["globals" .= parsed]
    _ -> pure $ mkCommandError ["ls", "globals"] so

callStackUnpack :: Maybe FilePath -> Value -> IO ToolResult
callStackUnpack mcwd params = do
  let pkg = getParamText "package" params
  if T.null pkg
    then pure $ mkToolError "package parameter is required"
    else do
      let args = ["unpack", pkg]
      so <- runStackBuild mcwd args
      pure $ case soExitCode so of
        0 -> mkToolResultJSON $ object
          ["success" .= True, "package" .= pkg, "output" .= soStdout so]
        _ -> mkCommandError args so

callStackUpdate :: Maybe FilePath -> IO ToolResult
callStackUpdate mcwd = do
  let args = ["update"]
  so <- runStackBuild mcwd args
  pure $ case soExitCode so of
    0 -> mkToolResultJSON $ object ["success" .= True, "output" .= soStdout so]
    _ -> mkCommandError args so

callStackList :: Maybe FilePath -> Value -> IO ToolResult
callStackList mcwd params = do
  let pkg      = getParamText "package" params
      snapshot = getParamText "snapshot" params
      args = ["list"]
          ++ [pkg | not (T.null pkg)]
          ++ ["--snapshot" | not (T.null snapshot)] ++ [snapshot | not (T.null snapshot)]
  so <- runStackBuild mcwd args
  case soExitCode so of
    0 -> do
      let ls = filter (not . T.null) $ T.lines (soStdout so)
      pure $ mkToolResultJSON $ object
        [ "count"    .= length ls
        , "packages" .= ls
        ]
    _ -> pure $ mkCommandError args so

callStackDot :: Maybe FilePath -> Value -> IO ToolResult
callStackDot mcwd params = do
  let targets  = T.words (getParamText "targets" params)
      external = getParamBool "external" params
      depth    = getParamInt "depth" params
      prune    = getParamText "prune" params
      flags    = T.words (getParamText "flags" params)
      depthArg = case depth of
        Just n  -> ["--depth", T.pack (show n)]
        Nothing -> []
      args = ["dot"] ++ targets
          ++ ["--external" | external]
          ++ depthArg
          ++ ["--prune" | not (T.null prune)] ++ [prune | not (T.null prune)]
          ++ flags
  so <- runStackBuild mcwd args
  pure $ case soExitCode so of
    0 -> mkToolResultJSON $ object ["dot" .= soStdout so, "format" .= ("graphviz" :: Text)]
    _ -> mkCommandError args so

callStackSdist :: Maybe FilePath -> Value -> IO ToolResult
callStackSdist mcwd params = do
  let targets = T.words (getParamText "targets" params)
      flags   = T.words (getParamText "flags" params)
      args    = ["sdist"] ++ targets ++ flags
  so <- runStackBuild mcwd args
  pure $ case soExitCode so of
    0 -> mkToolResultJSON $ object ["success" .= True, "output" .= soStdout so]
    _ -> mkCommandError args so

callStackUpload :: Maybe FilePath -> Value -> IO ToolResult
callStackUpload mcwd params = do
  let targets = T.words (getParamText "targets" params)
      docs    = getParamBool "documentation" params
      cand    = getParamBool "candidate" params
      flags   = T.words (getParamText "flags" params)
      args = ["upload"] ++ targets
          ++ ["--documentation" | docs]
          ++ ["--candidate" | cand]
          ++ flags
  so <- runStackBuild mcwd args
  pure $ case soExitCode so of
    0 -> mkToolResultJSON $ object ["success" .= True, "output" .= soStdout so]
    _ -> mkCommandError args so

------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------

splitComma :: Text -> [Text]
splitComma t
  | T.null t  = []
  | otherwise = map T.strip (T.splitOn "," t)
