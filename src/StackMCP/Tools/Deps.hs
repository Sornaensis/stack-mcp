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
  , stackLsSnapshotsDef
  , stackLsGlobalsDef
  , stackUpdateDef
  ]

dispatch :: Maybe FilePath -> Text -> Value -> Maybe (IO ToolResult)
dispatch mcwd name params = case name of
  "stack_ls_dependencies" -> Just $ callStackLsDeps mcwd params
  "stack_ls_snapshots"    -> Just $ callStackLsSnapshots mcwd params
  "stack_ls_globals"      -> Just $ callStackLsGlobals mcwd
  "stack_update"          -> Just $ callStackUpdate mcwd
  _                       -> Nothing

------------------------------------------------------------------------
-- Definitions
------------------------------------------------------------------------

stackLsDepsDef :: ToolDef
stackLsDepsDef = ToolDef "stack_ls_dependencies"
  "List project dependencies. Use format param to choose output style: \
  \text (default, parsed name/version pairs), json (Stack's native JSON), \
  \or tree (indented visual hierarchy)." $
  mkSchema
    [ ("format", enumProp "Output format." ["text", "json", "tree"])
    , ("depth", intProp "Maximum depth of dependency tree.")
    , ("license", boolProp "Print license instead of version (--license). Text and tree formats only.")
    , ("external", boolProp "Include only external dependencies (--external).")
    , ("separator", strProp "Separator between package name and version (default: space). Text format only.")
    , ("filter", strProp "Package name or $locals to filter out (can repeat, comma-separated). Text format only.")
    , ("prune", strProp "Comma-separated packages to prune from the tree.")
    , ("test", boolProp "Consider dependencies of test components (--test).")
    , ("bench", boolProp "Consider dependencies of benchmark components (--bench).")
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

stackUpdateDef :: ToolDef
stackUpdateDef = ToolDef "stack_update"
  "Update the Hackage package index." $
  mkSchema [] []

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
  let fmt = getParamText "format" params
  case fmt of
    "json" -> callLsDepsJson mcwd params
    "tree" -> callLsDepsTree mcwd params
    _      -> callLsDepsText mcwd params

callLsDepsText :: Maybe FilePath -> Value -> IO ToolResult
callLsDepsText mcwd params = do
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
        [ "dependencies" .= parsed
        ]
    _ -> pure $ mkCommandError args so

callLsDepsJson :: Maybe FilePath -> Value -> IO ToolResult
callLsDepsJson mcwd params = do
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

callLsDepsTree :: Maybe FilePath -> Value -> IO ToolResult
callLsDepsTree mcwd params = do
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

callStackUpdate :: Maybe FilePath -> IO ToolResult
callStackUpdate mcwd = do
  let args = ["update"]
  so <- runStackBuild mcwd args
  pure $ case soExitCode so of
    0 -> mkToolResultJSON $ object ["output" .= soStdout so]
    _ -> mkCommandError args so

------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------

splitComma :: Text -> [Text]
splitComma t
  | T.null t  = []
  | otherwise = map T.strip (T.splitOn "," t)
