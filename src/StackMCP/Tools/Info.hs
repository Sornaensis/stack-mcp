{-# LANGUAGE OverloadedStrings #-}

module StackMCP.Tools.Info
  ( tools
  , dispatch
  ) where

import Data.Aeson.Key (fromText)
import Data.Text qualified as T
import StackMCP.Tools.Common

tools :: [ToolDef]
tools =
  [ stackPathDef
  , stackLsToolsDef
  , stackIdeTargetsDef
  , stackIdePackagesDef
  , stackUpgradeDef
  ]

dispatch :: Maybe FilePath -> Text -> Value -> Maybe (IO ToolResult)
dispatch mcwd name params = case name of
  "stack_path"        -> Just $ callStackPath mcwd params
  "stack_ls_tools"    -> Just $ callStackLsTools mcwd
  "stack_ide_targets" -> Just $ callStackIdeTargets mcwd params
  "stack_ide_packages" -> Just $ callStackIdePackages mcwd
  "stack_upgrade"     -> Just $ callStackUpgrade mcwd params
  _                   -> Nothing

------------------------------------------------------------------------
-- Definitions
------------------------------------------------------------------------

stackPathDef :: ToolDef
stackPathDef = ToolDef "stack_path"
  "Print Stack path information. Without a key, returns all paths as structured JSON." $
  mkSchema
    [ ("key", enumProp "Specific path key to query."
        [ "stack-root", "global-config", "programs", "local-bin"
        , "project-root", "config-location", "bin-path"
        , "compiler-exe", "compiler-bin", "compiler-tools-bin"
        , "extra-include-dirs", "extra-library-dirs"
        , "snapshot-pkg-db", "local-pkg-db", "global-pkg-db", "ghc-package-path"
        , "snapshot-install-root", "local-install-root"
        , "snapshot-doc-root", "local-doc-root", "local-hoogle-root"
        , "dist-dir", "local-hpc-root"
        ])
    ] []

stackLsToolsDef :: ToolDef
stackLsToolsDef = ToolDef "stack_ls_tools"
  "List tools installed by Stack." $
  mkSchema [] []

stackIdeTargetsDef :: ToolDef
stackIdeTargetsDef = ToolDef "stack_ide_targets"
  "List the project's build targets. Useful for IDE integration and discovering component names." $
  mkSchema
    [ ("exes", boolProp "Include executables (--exes).")
    , ("tests", boolProp "Include test suites (--tests).")
    , ("benchmarks", boolProp "Include benchmarks (--benchmarks).")
    ] []

stackIdePackagesDef :: ToolDef
stackIdePackagesDef = ToolDef "stack_ide_packages"
  "List all available local loadable packages." $
  mkSchema [] []

stackUpgradeDef :: ToolDef
stackUpgradeDef = ToolDef "stack_upgrade"
  "Upgrade Stack to the latest version." $
  mkSchema
    [ ("binary_only", boolProp "Only download a binary, don't build from source (--binary-only).")
    ] []

------------------------------------------------------------------------
-- Implementations
------------------------------------------------------------------------

callStackPath :: Maybe FilePath -> Value -> IO ToolResult
callStackPath mcwd params = do
  let key = getParamText "key" params
  if T.null key
    then do
      -- No key: get all paths, parse into structured JSON
      so <- runStackRaw mcwd ["path"]
      case soExitCode so of
        0 -> do
          let ls = filter (not . T.null) $ T.lines (soStdout so)
              kvPairs = [ (k, T.strip v)
                      | line <- ls
                      , let (k, rest) = T.breakOn ":" line
                      , not (T.null rest)
                      , let v = T.drop 1 rest
                      ]
          pure $ mkToolResultJSON $ object
            [ fromText k .= v | (k, v) <- kvPairs ]
        _ -> pure $ mkCommandError ["path"] so
    else do
      so <- runStackRaw mcwd ["path", "--" <> key]
      case soExitCode so of
        0 -> pure $ mkToolResultJSON $ object
               [ "key"   .= key
               , "value" .= T.strip (soStdout so)
               ]
        _ -> pure $ mkCommandError ["path", "--" <> key] so

callStackLsTools :: Maybe FilePath -> IO ToolResult
callStackLsTools mcwd = do
  so <- runStackRaw mcwd ["ls", "tools"]
  case soExitCode so of
    0 -> do
      let ls = filter (not . T.null) $ T.lines (soStdout so)
      pure $ mkToolResultJSON $ object ["tools" .= ls]
    _ -> pure $ mkCommandError ["ls", "tools"] so

callStackIdeTargets :: Maybe FilePath -> Value -> IO ToolResult
callStackIdeTargets mcwd params = do
  let exes    = getParamBool "exes" params
      tests   = getParamBool "tests" params
      benchs  = getParamBool "benchmarks" params
      args = ["ide", "targets", "--stdout"]
          ++ ["--exes" | exes]
          ++ ["--tests" | tests]
          ++ ["--benchmarks" | benchs]
  so <- runStackRaw mcwd args
  case soExitCode so of
    0 -> do
      let ls = filter (not . T.null) $ T.lines (soStdout so)
      pure $ mkToolResultJSON $ object
        [ "targets" .= ls
        ]
    _ -> pure $ mkCommandError args so

callStackIdePackages :: Maybe FilePath -> IO ToolResult
callStackIdePackages mcwd = do
  so <- runStackRaw mcwd ["ide", "packages"]
  case soExitCode so of
    0 -> do
      let ls = filter (not . T.null) $ T.lines (soStdout so)
      pure $ mkToolResultJSON $ object
        [ "packages" .= ls
        ]
    _ -> pure $ mkCommandError ["ide", "packages"] so

callStackUpgrade :: Maybe FilePath -> Value -> IO ToolResult
callStackUpgrade mcwd params = do
  let binOnly = getParamBool "binary_only" params
      args = ["upgrade"] ++ ["--binary-only" | binOnly]
  so <- runStackRaw mcwd args
  pure $ case soExitCode so of
    0 -> mkToolResultJSON $ object ["output" .= soStdout so]
    _ -> mkCommandError args so
