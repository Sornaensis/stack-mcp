{-# LANGUAGE OverloadedStrings #-}

module StackMCP.Tools.Testing
  ( tools
  , dispatch
  , parsedCounts
  , parseHspecCounts
  , parseTastyCounts
  ) where

import Data.Text qualified as T
import StackMCP.Tools.Common
import StackMCP.Tools.Parse (parseTestFailures, testFailuresSummary, parseGhcDiagnostics, diagnosticsSummary, readInt)

tools :: [ToolDef]
tools =
  [ stackTestDiscoverDef
  , stackTestRunDef
  , stackBenchDiscoverDef
  , stackBenchRunDef
  ]

dispatch :: Maybe FilePath -> Text -> Value -> Maybe (IO ToolResult)
dispatch mcwd name params = case name of
  "stack_test_discover"  -> Just $ callTestDiscover mcwd params
  "stack_test_run"       -> Just $ callTestRun mcwd params
  "stack_bench_discover" -> Just $ callBenchDiscover mcwd params
  "stack_bench_run"      -> Just $ callBenchRun mcwd params
  _                      -> Nothing

------------------------------------------------------------------------
-- Definitions
------------------------------------------------------------------------

stackTestDiscoverDef :: ToolDef
stackTestDiscoverDef = ToolDef "stack_test_discover"
  "Discover available test suites and optionally list individual test cases within a suite. \
  \Without a suite, returns all test suite targets. With a suite, attempts to list its \
  \individual test cases via --list-tests (tasty) or --dry-run (hspec)." $
  mkSchema
    [ ("suite", strProp "Specific test suite target to inspect (e.g. package:test:suite-name). If omitted, lists all test suites.")
    , ("framework", enumProp "Test framework hint for listing individual tests." ["tasty", "hspec", "auto"])
    ] []

stackTestRunDef :: ToolDef
stackTestRunDef = ToolDef "stack_test_run"
  "Run a specific test suite, optionally filtering to individual tests by pattern. \
  \Returns structured JSON with success status, output, and parsed test counts when available." $
  mkSchema
    [ ("suite", strProp "Test suite target (e.g. package:test:suite-name). Required.")
    , ("match", strProp "Pattern to match specific tests. Passed as --match (hspec) or -p (tasty).")
    , ("framework", enumProp "Test framework hint for argument format." ["tasty", "hspec", "auto"])
    , ("coverage", boolProp "Enable code coverage (--coverage).")
    , ("ta", strProp "Raw test arguments passed via --ta.")
    , ("flags", strProp "Additional raw flags for stack test.")
    ] ["suite"]

stackBenchDiscoverDef :: ToolDef
stackBenchDiscoverDef = ToolDef "stack_bench_discover"
  "Discover available benchmark suites in the project. Returns structured JSON with benchmark targets." $
  mkSchema [] []

stackBenchRunDef :: ToolDef
stackBenchRunDef = ToolDef "stack_bench_run"
  "Run a specific benchmark suite with structured output. \
  \Returns JSON with success, output, and timing data when parseable." $
  mkSchema
    [ ("suite", strProp "Benchmark suite target (e.g. package:bench:bench-name). Required.")
    , ("match", strProp "Pattern to match specific benchmarks (criterion: passed via --ba \"--match pattern\").")
    , ("ba", strProp "Raw benchmark arguments passed via --ba.")
    , ("flags", strProp "Additional raw flags for stack bench.")
    ] ["suite"]

------------------------------------------------------------------------
-- Implementations
------------------------------------------------------------------------

callTestDiscover :: Maybe FilePath -> Value -> IO ToolResult
callTestDiscover mcwd params = do
  let suite     = getParamText "suite" params
      framework = getParamText "framework" params
  if T.null suite
    then do
      -- List all test suites
      so <- runStackBuild mcwd ["ide", "targets", "--stdout", "--tests"]
      case soExitCode so of
        0 -> do
          let targets = filter (not . T.null) $ T.lines (soStdout so)
              parsed  = map parseTarget targets
          pure $ mkToolResultJSON $ object
            [ "count"   .= length parsed
            , "suites"  .= parsed
            ]
        _ -> pure $ mkCommandError ["ide", "targets", "--stdout", "--tests"] so
    else do
      -- Try to list individual test cases within a suite
      let listArgs = case framework of
            "tasty" -> ["--list-tests"]
            "hspec" -> ["--dry-run", "--format", "checks"]
            _       -> ["--list-tests"]  -- try tasty-style first
          taFlags = tagEach "--ta" listArgs
      so <- runStackBuild mcwd $ ["test", suite] ++ taFlags
      case soExitCode so of
        0 -> do
          let rawLines = filter (not . T.null) $ T.lines (soStdout so)
              tests    = map T.strip rawLines
          pure $ mkToolResultJSON $ object
            [ "suite"  .= suite
            , "count"  .= length tests
            , "tests"  .= tests
            ]
        _ -> do
          -- --list-tests may have failed; try hspec --dry-run as fallback
          -- Only attempt hspec fallback when framework is "auto" or unset
          if framework == "" || framework == "auto"
            then do
              so2 <- runStackBuild mcwd
                ["test", suite, "--ta", "--dry-run", "--ta", "--format", "--ta", "checks"]
              case soExitCode so2 of
                0 -> do
                  let rawLines = filter (not . T.null) $ T.lines (soStdout so2)
                  pure $ mkToolResultJSON $ object
                    [ "suite"  .= suite
                    , "count"  .= length rawLines
                    , "tests"  .= map T.strip rawLines
                    , "note"   .= ("Discovered via hspec --dry-run fallback" :: Text)
                    ]
                _ -> pure $ mkToolError $
                  "Could not list tests for " <> suite
                  <> ". Try running the suite with stack_test_run instead.\n"
                  <> soStdout so <> "\n" <> soStderr so
            else pure $ mkToolError $
              "Could not list tests for " <> suite <> ".\n"
              <> soStdout so <> "\n" <> soStderr so

callTestRun :: Maybe FilePath -> Value -> IO ToolResult
callTestRun mcwd params = do
  let suite     = getParamText "suite" params
      match     = getParamText "match" params
      framework = getParamText "framework" params
      coverage  = getParamBool "coverage" params
      rawTa     = getParamText "ta" params
      flags     = T.words (getParamText "flags" params)
  if T.null suite
    then pure $ mkToolError "suite parameter is required"
    else do
      let matchArgs = if T.null match
            then []
            else case framework of
              "tasty" -> ["-p", match]
              "hspec" -> ["--match", match]
              _       -> ["--match", match]  -- default to hspec-style
          taWords = matchArgs ++ T.words rawTa
          args = ["test", suite]
              ++ ["--coverage" | coverage]
              ++ tagEach "--ta" taWords
              ++ flags
      so <- runStackBuild mcwd args
      let success    = soExitCode so == 0
          combined   = soStdout so <> "\n" <> soStderr so
          failures   = parseTestFailures combined
          diags      = parseGhcDiagnostics (soStderr so)
          rootField  = maybe [] (\d -> ["project_root" .= T.pack d]) mcwd
          result     = object $
            [ "success" .= success
            , "suite"   .= suite
            , "output"  .= soStdout so
            , "stderr"  .= soStderr so
            ] ++ rootField
              ++ parsedCounts combined
              ++ (if null failures then []
                  else ["test_failures" .= testFailuresSummary failures])
              ++ (if null diags then []
                  else ["diagnostics" .= diagnosticsSummary diags])
      pure $ if success
        then mkToolResultJSON result
        else mkToolErrorJSON result

callBenchDiscover :: Maybe FilePath -> Value -> IO ToolResult
callBenchDiscover mcwd _params = do
  so <- runStackBuild mcwd ["ide", "targets", "--stdout", "--benchmarks"]
  case soExitCode so of
    0 -> do
      let targets = filter (not . T.null) $ T.lines (soStdout so)
          parsed  = map parseTarget targets
      pure $ mkToolResultJSON $ object
        [ "count"  .= length parsed
        , "suites" .= parsed
        ]
    _ -> pure $ mkCommandError ["ide", "targets", "--stdout", "--benchmarks"] so

callBenchRun :: Maybe FilePath -> Value -> IO ToolResult
callBenchRun mcwd params = do
  let suite  = getParamText "suite" params
      match  = getParamText "match" params
      rawBa  = getParamText "ba" params
      flags  = T.words (getParamText "flags" params)
  if T.null suite
    then pure $ mkToolError "suite parameter is required"
    else do
      let matchArgs = if T.null match then [] else ["--match", match]
          baWords = matchArgs ++ T.words rawBa
          args = ["bench", suite]
              ++ tagEach "--ba" baWords
              ++ flags
      so <- runStackBuild mcwd args
      let success  = soExitCode so == 0
          combined = soStdout so <> "\n" <> soStderr so
          diags    = parseGhcDiagnostics (soStderr so)
          rootField = maybe [] (\d -> ["project_root" .= T.pack d]) mcwd
          result   = object $
            [ "success" .= success
            , "suite"   .= suite
            , "output"  .= soStdout so
            , "stderr"  .= soStderr so
            ] ++ rootField
              ++ parsedCounts combined
              ++ (if null diags then []
                  else ["diagnostics" .= diagnosticsSummary diags])
      pure $ if success
        then mkToolResultJSON result
        else mkToolErrorJSON result

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Parse a target string like "package:test:suite-name" into structured JSON.
parseTarget :: Text -> Value
parseTarget t =
  let parts = T.splitOn ":" t
  in case parts of
    [pkg, kind, name] -> object
      [ "target"  .= t
      , "package" .= pkg
      , "kind"    .= kind
      , "name"    .= name
      ]
    [pkg, name] -> object
      [ "target"  .= t
      , "package" .= pkg
      , "name"    .= name
      ]
    _ -> object ["target" .= t]

-- | Try to extract test counts from combined output.
--   Looks for common patterns like "N examples, M failures" (hspec)
--   or "N out of N tests passed" (tasty).
parsedCounts :: Text -> [(Key, Value)]
parsedCounts output =
  let ls = T.lines output
      hspecLine = findLine "examples" ls
      tastyLine = findLine "tests passed" ls
  in case hspecLine of
    Just l  -> parseHspecCounts l
    Nothing -> case tastyLine of
      Just l  -> parseTastyCounts l
      Nothing -> []

findLine :: Text -> [Text] -> Maybe Text
findLine needle = go
  where
    go []     = Nothing
    go (l:ls)
      | T.isInfixOf needle l = Just l
      | otherwise             = go ls

parseHspecCounts :: Text -> [(Key, Value)]
parseHspecCounts l =
  let ws = T.words l
      nums = extractNums ws
  in case nums of
    (examples:failures:pending:_) ->
      [ "total"    .= examples
      , "failures" .= failures
      , "pending"  .= pending
      , "passed"   .= (examples - failures - pending)
      ]
    (examples:failures:_) ->
      [ "total"    .= examples
      , "failures" .= failures
      , "passed"   .= (examples - failures)
      ]
    _ -> []

parseTastyCounts :: Text -> [(Key, Value)]
parseTastyCounts l =
  let ws = T.words l
      nums = extractNums ws
  in case nums of
    (passed:total:_) ->
      [ "total"    .= total
      , "passed"   .= passed
      , "failures" .= (total - passed)
      ]
    _ -> []

extractNums :: [Text] -> [Int]
extractNums = foldr (\w acc -> case readInt w of Just n -> n : acc; Nothing -> acc) []
