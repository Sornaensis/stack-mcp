{-# LANGUAGE OverloadedStrings #-}

module StackMCP.Tools.Build
  ( tools
  , dispatch
  ) where

import Data.Text qualified as T
import StackMCP.Tools.Common
import StackMCP.Tools.Parse (parseGhcDiagnostics, filteredDiagnosticsSummary, parseDepErrors, depErrorsSummary, parseTestFailures, testFailuresSummary)
import StackMCP.Tools.Testing (parsedCounts)

tools :: [ToolDef]
tools =
  [ stackBuildDef
  , stackTestDef
  , stackBenchDef
  , stackHaddockDef
  , stackInstallDef
  , stackRunDef
  , stackCleanDef
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
    , ("no_link", boolProp "Type-check only, skip code generation (-fno-code). Combine with fast=true for a quick typecheck-only loop.")
    , ("file_watch", boolProp "Watch for file changes and rebuild (--file-watch). NOTE: long-running — use task_exec for background watch mode.")
    , ("ghc_options", strProp "Additional GHC options (e.g. \"-O2 -fprof-auto\").")
    , ("flags", strProp "Additional raw flags to pass to stack build.")
    , ("include_warnings", boolProp "Include GHC warnings in the response (default: false). Only errors are returned otherwise.")
    , ("include_output", boolProp "Include raw stdout/stderr in the response (default: false).")
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
    , ("include_warnings", boolProp "Include GHC warnings in the response (default: false).")
    , ("include_output", boolProp "Include raw stdout/stderr in the response (default: false).")
    ] []

stackBenchDef :: ToolDef
stackBenchDef = ToolDef "stack_bench"
  "Run all benchmarks in the Stack project (shortcut for build --bench). \
  \To run a specific benchmark suite with pattern matching, use stack_bench_run instead (via @stack-build)." $
  mkSchema
    [ ("targets", strProp "Space-separated benchmark targets.")
    , ("ba", strProp "Benchmark arguments passed via --ba.")
    , ("flags", strProp "Additional raw flags.")
    , ("include_warnings", boolProp "Include GHC warnings in the response (default: false).")
    , ("include_output", boolProp "Include raw stdout/stderr in the response (default: false).")
    ] []

stackHaddockDef :: ToolDef
stackHaddockDef = ToolDef "stack_haddock"
  "Generate Haddock documentation for the project." $
  mkSchema
    [ ("targets", strProp "Space-separated targets.")
    , ("open", boolProp "Open the generated docs (--open).")
    , ("no_deps", boolProp "Skip documentation for dependencies (--no-haddock-deps).")
    , ("flags", strProp "Additional raw flags.")
    , ("include_warnings", boolProp "Include GHC warnings in the response (default: false).")
    , ("include_output", boolProp "Include raw stdout/stderr in the response (default: false).")
    ] []

stackInstallDef :: ToolDef
stackInstallDef = ToolDef "stack_install"
  "Build and copy executables to local-bin-path (shortcut for build --copy-bins)." $
  mkSchema
    [ ("targets", strProp "Space-separated targets to install.")
    , ("flags", strProp "Additional raw flags.")
    , ("include_warnings", boolProp "Include GHC warnings in the response (default: false).")
    , ("include_output", boolProp "Include raw stdout/stderr in the response (default: false).")
    ] []

stackRunDef :: ToolDef
stackRunDef = ToolDef "stack_run"
  "Build and run an executable, returning when it exits. Defaults to the first available executable. \
  \For long-running processes that need background monitoring, use task_run instead (via @stack-tasks)." $
  mkSchema
    [ ("executable", strProp "Name of the executable to run.")
    , ("args", strProp "Space-separated arguments passed to the executable.")
    , ("flags", strProp "Additional raw flags for stack run.")
    , ("include_warnings", boolProp "Include GHC warnings in the response (default: false).")
    , ("include_output", boolProp "Include raw stdout/stderr in the response (default: false).")
    ] []

stackCleanDef :: ToolDef
stackCleanDef = ToolDef "stack_clean"
  "Delete build artifacts. By default removes project packages only. \
  \Use full=true for the entire .stack-work directory, or purge=true for a \
  \full reset including all working directories and snapshots." $
  mkSchema
    [ ("full", boolProp "Delete the entire .stack-work directory (--full).")
    , ("purge", boolProp "Delete all .stack-work directories including snapshots (runs stack purge). Overrides full.")
    ] []

------------------------------------------------------------------------
-- Implementations
------------------------------------------------------------------------

-- | Parse build output into structured JSON with diagnostics.
--   Includes project_root so agents can resolve relative diagnostic paths.
--   Warnings are only included when @includeWarnings@ is True.
--   Raw output is only included when @includeOutput@ is True.
structuredBuild :: Maybe FilePath -> Bool -> Bool -> [Text] -> IO ToolResult
structuredBuild mcwd includeWarnings includeOutput args = do
  so <- runStackBuild mcwd args
  let diags = parseGhcDiagnostics (soStderr so)
      depErrs = parseDepErrors (soStderr so)
      mDiagSummary = filteredDiagnosticsSummary includeWarnings diags
  pure $ case soExitCode so of
    0 -> mkToolResultJSON $ object $
           maybe [] (\d -> ["diagnostics" .= d]) mDiagSummary
             ++ ["output" .= soStdout so | includeOutput]
    _ -> mkCommandErrorFiltered includeWarnings includeOutput args so diags depErrs mcwd

callStackBuild :: Maybe FilePath -> Value -> IO ToolResult
callStackBuild mcwd params = do
  let targets = T.words (getParamText "targets" params)
      fast    = getParamBool "fast" params
      ped     = getParamBool "pedantic" params
      noLink  = getParamBool "no_link" params
      watch   = getParamBool "file_watch" params
      ghcOpts = getParamText "ghc_options" params
      flags   = T.words (getParamText "flags" params)
      inclW   = getParamBool "include_warnings" params
      inclO   = getParamBool "include_output" params
      args = ["build"] ++ targets
          ++ ["--fast" | fast]
          ++ ["--pedantic" | ped]
          ++ ["--ghc-options" | noLink] ++ ["-fno-code" | noLink]
          ++ ["--file-watch" | watch]
          ++ ["--ghc-options" | not (T.null ghcOpts)] ++ [ghcOpts | not (T.null ghcOpts)]
          ++ flags
  structuredBuild mcwd inclW inclO args

-- | Like 'structuredBuild' but also parses test/benchmark counts and
--   test failures from the combined output.  Used by stack_test and
--   stack_bench so the model gets structured results by default.
structuredTest :: Maybe FilePath -> Bool -> Bool -> [Text] -> IO ToolResult
structuredTest mcwd includeWarnings includeOutput args = do
  so <- runStackBuild mcwd args
  let diags   = parseGhcDiagnostics (soStderr so)
      depErrs = parseDepErrors (soStderr so)
      mDiagSummary = filteredDiagnosticsSummary includeWarnings diags
      combined  = soStdout so <> "\n" <> soStderr so
      failures  = parseTestFailures combined
      counts    = parsedCounts combined
  pure $ case soExitCode so of
    0 -> mkToolResultJSON $ object $
           maybe [] (\d -> ["diagnostics" .= d]) mDiagSummary
             ++ counts
             ++ ["output" .= soStdout so | includeOutput]
    _ -> mkToolErrorJSON $ object $
           [ "error_type" .= ("command_failed" :: Text)
           , "exit_code"  .= soExitCode so
           , "command"    .= T.unwords ("stack" : args)
           ] ++ counts
             ++ (if null failures then []
                 else ["test_failures" .= testFailuresSummary failures])
             ++ maybe [] (\d -> ["diagnostics" .= d]) mDiagSummary
             ++ ["dependency_errors" .= depErrorsSummary depErrs | not (null depErrs)]
             ++ ["raw_output" .= soStdout so | includeOutput]
             ++ ["raw_stderr" .= soStderr so | includeOutput || (null diags && null depErrs && null failures)]
             ++ maybe [] (\d -> ["project_root" .= T.pack d]) mcwd

callStackTest :: Maybe FilePath -> Value -> IO ToolResult
callStackTest mcwd params = do
  let targets  = T.words (getParamText "targets" params)
      coverage = getParamBool "coverage" params
      ta       = getParamText "ta" params
      flags    = T.words (getParamText "flags" params)
      inclW    = getParamBool "include_warnings" params
      inclO    = getParamBool "include_output" params
      args = ["test"] ++ targets
          ++ ["--coverage" | coverage]
          ++ ["--ta" | not (T.null ta)] ++ [ta | not (T.null ta)]
          ++ flags
  structuredTest mcwd inclW inclO args

callStackBench :: Maybe FilePath -> Value -> IO ToolResult
callStackBench mcwd params = do
  let targets = T.words (getParamText "targets" params)
      ba      = getParamText "ba" params
      flags   = T.words (getParamText "flags" params)
      inclW   = getParamBool "include_warnings" params
      inclO   = getParamBool "include_output" params
      args = ["bench"] ++ targets
          ++ ["--ba" | not (T.null ba)] ++ [ba | not (T.null ba)]
          ++ flags
  structuredTest mcwd inclW inclO args

callStackHaddock :: Maybe FilePath -> Value -> IO ToolResult
callStackHaddock mcwd params = do
  let targets = T.words (getParamText "targets" params)
      doOpen  = getParamBool "open" params
      noDeps  = getParamBool "no_deps" params
      flags   = T.words (getParamText "flags" params)
      inclW   = getParamBool "include_warnings" params
      inclO   = getParamBool "include_output" params
      args = ["haddock"] ++ targets
          ++ ["--open" | doOpen]
          ++ ["--no-haddock-deps" | noDeps]
          ++ flags
  structuredBuild mcwd inclW inclO args

callStackInstall :: Maybe FilePath -> Value -> IO ToolResult
callStackInstall mcwd params = do
  let targets = T.words (getParamText "targets" params)
      flags   = T.words (getParamText "flags" params)
      inclW   = getParamBool "include_warnings" params
      inclO   = getParamBool "include_output" params
      args    = ["install"] ++ targets ++ flags
  structuredBuild mcwd inclW inclO args

callStackRun :: Maybe FilePath -> Value -> IO ToolResult
callStackRun mcwd params = do
  let exe   = getParamText "executable" params
      rArgs = T.words (getParamText "args" params)
      flags = T.words (getParamText "flags" params)
      inclW = getParamBool "include_warnings" params
      inclO = getParamBool "include_output" params
      args = ["run"] ++ [exe | not (T.null exe)] ++ flags
          ++ (if null rArgs then [] else "--" : rArgs)
  so <- runStackRaw mcwd args
  let diags = parseGhcDiagnostics (soStderr so)
      depErrs = parseDepErrors (soStderr so)
      mDiagSummary = filteredDiagnosticsSummary inclW diags
  pure $ case soExitCode so of
    0 -> mkToolResultJSON $ object $
           ["output" .= soStdout so | inclO]
             ++ maybe [] (\d -> ["diagnostics" .= d]) mDiagSummary
    _ -> mkCommandErrorFiltered inclW inclO args so diags depErrs mcwd

callStackClean :: Maybe FilePath -> Value -> IO ToolResult
callStackClean mcwd params = do
  let purge = getParamBool "purge" params
      full  = getParamBool "full" params
      args | purge     = ["purge"]
           | otherwise = ["clean"] ++ ["--full" | full]
  so <- runStackBuild mcwd args
  pure $ case soExitCode so of
    0 -> mkToolResultJSON $ object ["output" .= soStdout so]
    _ -> mkCommandError args so
