{-# LANGUAGE OverloadedStrings #-}

module StackMCP.Tools.Build
  ( tools
  , dispatch
  ) where

import Data.Text qualified as T
import StackMCP.Tools.Common
import StackMCP.Tools.Parse (parseGhcDiagnostics, diagnosticsSummary, parseDepErrors)

tools :: [ToolDef]
tools =
  [ stackBuildDef
  , stackTestDef
  , stackBenchDef
  , stackHaddockDef
  , stackInstallDef
  , stackRunDef
  , stackCleanDef
  , stackPurgeDef
  , stackHpcReportDef
  , stackTypecheckDef
  ]

dispatch :: Maybe FilePath -> Text -> Value -> Maybe (IO ToolResult)
dispatch mcwd name params = case name of
  "stack_build"   -> Just $ callStackBuild mcwd params
  "stack_test"    -> Just $ callStackTest mcwd params
  "stack_bench"   -> Just $ callStackBench mcwd params
  "stack_haddock" -> Just $ callStackHaddock mcwd params
  "stack_install" -> Just $ callStackInstall mcwd params
  "stack_run"     -> Just $ callStackRun mcwd params
  "stack_clean"   -> Just $ callStackClean mcwd params
  "stack_purge"      -> Just $ callStackPurge mcwd
  "stack_hpc_report" -> Just $ callStackHpcReport mcwd params
  "stack_typecheck"  -> Just $ callStackTypecheck mcwd params
  _                  -> Nothing

------------------------------------------------------------------------
-- Definitions
------------------------------------------------------------------------

stackBuildDef :: ToolDef
stackBuildDef = ToolDef "stack_build"
  "Build the Stack project. Optionally specify targets and flags." $
  mkSchema
    [ ("targets", strProp "Space-separated build targets (e.g. package:lib, package:exe:name).")
    , ("fast", boolProp "Disable optimizations for faster compile (--fast).")
    , ("pedantic", boolProp "Enable -Wall -Werror (--pedantic).")
    , ("no_link", boolProp "Type-check only, skip code generation (-fno-code). Prefer stack_typecheck for the fix loop (auto-adds --fast).")
    , ("file_watch", boolProp "Watch for file changes and rebuild (--file-watch). NOTE: long-running — use task_exec for background watch mode.")
    , ("ghc_options", strProp "Additional GHC options (e.g. \"-O2 -fprof-auto\").")
    , ("flags", strProp "Additional raw flags to pass to stack build.")
    ] []

stackTestDef :: ToolDef
stackTestDef = ToolDef "stack_test"
  "Run all test suites in the Stack project (shortcut for build --test). \
  \To run a specific test suite with pattern matching, use stack_test_run instead (via @stack-build)." $
  mkSchema
    [ ("targets", strProp "Space-separated test targets.")
    , ("coverage", boolProp "Generate code coverage report (--coverage).")
    , ("ta", strProp "Test arguments passed via --ta (e.g. \"--match pattern\").")
    , ("flags", strProp "Additional raw flags.")
    ] []

stackBenchDef :: ToolDef
stackBenchDef = ToolDef "stack_bench"
  "Run all benchmarks in the Stack project (shortcut for build --bench). \
  \To run a specific benchmark suite with pattern matching, use stack_bench_run instead (via @stack-build)." $
  mkSchema
    [ ("targets", strProp "Space-separated benchmark targets.")
    , ("ba", strProp "Benchmark arguments passed via --ba.")
    , ("flags", strProp "Additional raw flags.")
    ] []

stackHaddockDef :: ToolDef
stackHaddockDef = ToolDef "stack_haddock"
  "Generate Haddock documentation for the project." $
  mkSchema
    [ ("targets", strProp "Space-separated targets.")
    , ("open", boolProp "Open the generated docs (--open).")
    , ("no_deps", boolProp "Skip documentation for dependencies (--no-haddock-deps).")
    , ("flags", strProp "Additional raw flags.")
    ] []

stackInstallDef :: ToolDef
stackInstallDef = ToolDef "stack_install"
  "Build and copy executables to local-bin-path (shortcut for build --copy-bins)." $
  mkSchema
    [ ("targets", strProp "Space-separated targets to install.")
    , ("flags", strProp "Additional raw flags.")
    ] []

stackRunDef :: ToolDef
stackRunDef = ToolDef "stack_run"
  "Build and run an executable, returning when it exits. Defaults to the first available executable. \
  \For long-running processes that need background monitoring, use task_run instead (via @stack-tasks)." $
  mkSchema
    [ ("executable", strProp "Name of the executable to run.")
    , ("args", strProp "Space-separated arguments passed to the executable.")
    , ("flags", strProp "Additional raw flags for stack run.")
    ] []

stackCleanDef :: ToolDef
stackCleanDef = ToolDef "stack_clean"
  "Delete build artifacts for project packages only. \
  \For a full reset including all .stack-work directories, use stack_purge instead." $
  mkSchema
    [ ("full", boolProp "Delete the entire .stack-work directory (--full).")
    ] []

stackPurgeDef :: ToolDef
stackPurgeDef = ToolDef "stack_purge"
  "Delete all project Stack working directories (.stack-work). \
  \More aggressive than stack_clean — removes everything including snapshots." $
  mkSchema [] []

stackTypecheckDef :: ToolDef
stackTypecheckDef = ToolDef "stack_typecheck"
  "Fast typecheck: build with --fast -fno-code. Skips optimizations and code generation \
  \for the fastest possible feedback on type errors. Ideal for the build-fix loop." $
  mkSchema
    [ ("targets", strProp "Space-separated build targets.")
    , ("ghc_options", strProp "Additional GHC options.")
    ] []

stackHpcReportDef :: ToolDef
stackHpcReportDef = ToolDef "stack_hpc_report"
  "Generate a unified HPC code coverage report from .tix files produced by a previous --coverage test run. \
  \Returns the report location. Use after stack_test with coverage=true." $
  mkSchema
    [ ("target", strProp "Specific target or .tix file. Omit to use the default project target.")
    , ("all", boolProp "Use results from all packages and components involved in previous --coverage run (--all).")
    , ("destdir", strProp "Output directory for the HTML report.")
    , ("open", boolProp "Open the report in the browser (--open).")
    ] []

------------------------------------------------------------------------
-- Implementations
------------------------------------------------------------------------

-- | Parse build output into structured JSON with diagnostics.
--   Includes project_root so agents can resolve relative diagnostic paths.
structuredBuild :: Maybe FilePath -> [Text] -> IO ToolResult
structuredBuild mcwd args = do
  so <- runStackRaw mcwd args
  let diags = parseGhcDiagnostics (soStderr so)
      depErrs = parseDepErrors (soStderr so)
      rootField = maybe [] (\d -> ["project_root" .= T.pack d]) mcwd
  pure $ case soExitCode so of
    0 -> mkToolResultJSON $ object $
           [ "success"     .= True
           , "output"      .= soStdout so
           , "diagnostics" .= diagnosticsSummary diags
           ] ++ rootField
    _ -> mkCommandErrorWithDiags args so diags depErrs mcwd

callStackBuild :: Maybe FilePath -> Value -> IO ToolResult
callStackBuild mcwd params = do
  let targets = T.words (getParamText "targets" params)
      fast    = getParamBool "fast" params
      ped     = getParamBool "pedantic" params
      noLink  = getParamBool "no_link" params
      watch   = getParamBool "file_watch" params
      ghcOpts = getParamText "ghc_options" params
      flags   = T.words (getParamText "flags" params)
      args = ["build"] ++ targets
          ++ ["--fast" | fast]
          ++ ["--pedantic" | ped]
          ++ ["--ghc-options" | noLink] ++ ["-fno-code" | noLink]
          ++ ["--file-watch" | watch]
          ++ ["--ghc-options" | not (T.null ghcOpts)] ++ [ghcOpts | not (T.null ghcOpts)]
          ++ flags
  structuredBuild mcwd args

callStackTest :: Maybe FilePath -> Value -> IO ToolResult
callStackTest mcwd params = do
  let targets  = T.words (getParamText "targets" params)
      coverage = getParamBool "coverage" params
      ta       = getParamText "ta" params
      flags    = T.words (getParamText "flags" params)
      args = ["test"] ++ targets
          ++ ["--coverage" | coverage]
          ++ ["--ta" | not (T.null ta)] ++ [ta | not (T.null ta)]
          ++ flags
  structuredBuild mcwd args

callStackBench :: Maybe FilePath -> Value -> IO ToolResult
callStackBench mcwd params = do
  let targets = T.words (getParamText "targets" params)
      ba      = getParamText "ba" params
      flags   = T.words (getParamText "flags" params)
      args = ["bench"] ++ targets
          ++ ["--ba" | not (T.null ba)] ++ [ba | not (T.null ba)]
          ++ flags
  structuredBuild mcwd args

callStackHaddock :: Maybe FilePath -> Value -> IO ToolResult
callStackHaddock mcwd params = do
  let targets = T.words (getParamText "targets" params)
      doOpen  = getParamBool "open" params
      noDeps  = getParamBool "no_deps" params
      flags   = T.words (getParamText "flags" params)
      args = ["haddock"] ++ targets
          ++ ["--open" | doOpen]
          ++ ["--no-haddock-deps" | noDeps]
          ++ flags
  structuredBuild mcwd args

callStackInstall :: Maybe FilePath -> Value -> IO ToolResult
callStackInstall mcwd params = do
  let targets = T.words (getParamText "targets" params)
      flags   = T.words (getParamText "flags" params)
      args    = ["install"] ++ targets ++ flags
  structuredBuild mcwd args

callStackRun :: Maybe FilePath -> Value -> IO ToolResult
callStackRun mcwd params = do
  let exe   = getParamText "executable" params
      rArgs = T.words (getParamText "args" params)
      flags = T.words (getParamText "flags" params)
      args = ["run"] ++ [exe | not (T.null exe)] ++ flags
          ++ (if null rArgs then [] else "--" : rArgs)
  so <- runStackRaw mcwd args
  let diags = parseGhcDiagnostics (soStderr so)
      depErrs = parseDepErrors (soStderr so)
  let rootField = maybe [] (\d -> ["project_root" .= T.pack d]) mcwd
  pure $ case soExitCode so of
    0 -> mkToolResultJSON $ object $
           [ "success"     .= True
           , "output"      .= soStdout so
           , "diagnostics" .= diagnosticsSummary diags
           ] ++ rootField
    _ -> mkCommandErrorWithDiags args so diags depErrs mcwd

callStackClean :: Maybe FilePath -> Value -> IO ToolResult
callStackClean mcwd params = do
  let full = getParamBool "full" params
      args = ["clean"] ++ ["--full" | full]
  so <- runStackRaw mcwd args
  pure $ case soExitCode so of
    0 -> mkToolResultJSON $ object ["success" .= True, "output" .= soStdout so]
    _ -> mkCommandError args so

callStackPurge :: Maybe FilePath -> IO ToolResult
callStackPurge mcwd = do
  so <- runStackRaw mcwd ["purge"]
  pure $ case soExitCode so of
    0 -> mkToolResultJSON $ object ["success" .= True, "output" .= soStdout so]
    _ -> mkCommandError ["purge"] so

callStackTypecheck :: Maybe FilePath -> Value -> IO ToolResult
callStackTypecheck mcwd params = do
  let targets = T.words (getParamText "targets" params)
      ghcOpts = getParamText "ghc_options" params
      args = ["build"] ++ targets
          ++ ["--fast"]
          ++ ["--ghc-options", "-fno-code"]
          ++ ["--ghc-options" | not (T.null ghcOpts)] ++ [ghcOpts | not (T.null ghcOpts)]
  structuredBuild mcwd args

callStackHpcReport :: Maybe FilePath -> Value -> IO ToolResult
callStackHpcReport mcwd params = do
  let target  = getParamText "target" params
      allPkgs = getParamBool "all" params
      destdir = getParamText "destdir" params
      doOpen  = getParamBool "open" params
      args = ["hpc", "report"]
          ++ [target | not (T.null target)]
          ++ ["--all" | allPkgs]
          ++ ["--destdir" | not (T.null destdir)] ++ [destdir | not (T.null destdir)]
          ++ ["--open" | doOpen]
  so <- runStackRaw mcwd args
  pure $ case soExitCode so of
    0 -> mkToolResultJSON $ object
           [ "success" .= True
           , "output"  .= soStdout so
           ]
    _ -> mkCommandError args so
