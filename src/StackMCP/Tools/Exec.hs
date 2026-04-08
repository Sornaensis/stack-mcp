{-# LANGUAGE OverloadedStrings #-}

module StackMCP.Tools.Exec
  ( tools
  , dispatch
  ) where

import Data.Text qualified as T
import StackMCP.Tools.Common
import StackMCP.Tools.Parse (parseGhcDiagnostics, filteredDiagnosticsSummary, parseDepErrors)

tools :: [ToolDef]
tools =
  [ stackExecDef
  , stackGhciDef
  , stackGhcDef
  , stackEvalDef
  , stackRunghcDef
  , stackScriptDef
  , stackHoogleDef
  ]

dispatch :: Maybe FilePath -> Text -> Value -> Maybe (IO ToolResult)
dispatch mcwd name params = case name of
  "stack_exec"   -> Just $ callStackExec mcwd params
  "stack_ghci"   -> Just $ callStackGhci mcwd params
  "stack_ghc"    -> Just $ callStackGhc mcwd params
  "stack_eval"   -> Just $ callStackEval mcwd params
  "stack_runghc" -> Just $ callStackRunghc mcwd params
  "stack_script" -> Just $ callStackScript mcwd params
  "stack_hoogle" -> Just $ callStackHoogle mcwd params
  _              -> Nothing

------------------------------------------------------------------------
-- Definitions
------------------------------------------------------------------------

stackExecDef :: ToolDef
stackExecDef = ToolDef "stack_exec"
  "Execute a one-shot command in the Stack environment (with Stack's PATH and GHC available). \
  \Returns immediately with exit code and output. For long-running or interactive processes, \
  \use task_exec instead (via @stack-tasks)." $
  mkSchema
    [ ("command", strProp "The command to execute (required).")
    , ("args", strProp "Space-separated arguments for the command.")
    , ("include_warnings", boolProp "Include GHC warnings in the response (default: false).")
    , ("include_output", boolProp "Include raw stdout/stderr in the response (default: false).")
    ] ["command"]

stackGhciDef :: ToolDef
stackGhciDef = ToolDef "stack_ghci"
  "Get GHCi information for the project (returns session info, does not start interactive session). \
  \For an interactive GHCi session, use task_ghci + task_ghci_eval instead (via @stack-tasks)." $
  mkSchema
    [ ("targets", strProp "Targets to load in GHCi.")
    , ("no_load", boolProp "Do not load modules at start (--no-load). Defaults to false (modules are loaded normally).")
    , ("flags", strProp "Additional raw flags.")
    , ("include_warnings", boolProp "Include GHC warnings in the response (default: false).")
    , ("include_output", boolProp "Include raw stdout/stderr in the response (default: false).")
    ] []

stackGhcDef :: ToolDef
stackGhcDef = ToolDef "stack_ghc"
  "Run GHC directly with arbitrary compiler arguments. \
  \For evaluating a Haskell expression, prefer stack_eval (simpler interface)." $
  mkSchema
    [ ("args", strProp "Arguments to pass to GHC (e.g. \"--version\", \"-e 'print 42'\").")
    , ("include_warnings", boolProp "Include GHC warnings in the response (default: false).")
    , ("include_output", boolProp "Include raw stdout/stderr in the response (default: false).")
    ] ["args"]

stackEvalDef :: ToolDef
stackEvalDef = ToolDef "stack_eval"
  "Evaluate a single Haskell expression and return the result (shortcut for stack ghci -e). \
  \Prefer this over stack_ghc for simple expression evaluation." $
  mkSchema
    [ ("expression", strProp "Haskell expression to evaluate (required).")
    , ("include_warnings", boolProp "Include GHC warnings in the response (default: false).")
    , ("include_output", boolProp "Include raw stdout/stderr in the response (default: false).")
    ] ["expression"]

stackRunghcDef :: ToolDef
stackRunghcDef = ToolDef "stack_runghc"
  "Run a Haskell source file with runghc (uses project dependencies). \
  \For self-contained scripts with their own resolver, use stack_script instead." $
  mkSchema
    [ ("file", strProp "Path to the Haskell source file (required).")
    , ("args", strProp "Additional arguments.")
    , ("include_warnings", boolProp "Include GHC warnings in the response (default: false).")
    , ("include_output", boolProp "Include raw stdout/stderr in the response (default: false).")
    ] ["file"]

stackScriptDef :: ToolDef
stackScriptDef = ToolDef "stack_script"
  "Run a self-contained Haskell script with its own resolver (requires {- stack ... -} header or --resolver). \
  \For running a file that uses project dependencies, use stack_runghc instead." $
  mkSchema
    [ ("file", strProp "Path to the script file (required).")
    , ("resolver", strProp "Snapshot resolver for the script.")
    , ("args", strProp "Additional arguments.")
    , ("include_warnings", boolProp "Include GHC warnings in the response (default: false).")
    , ("include_output", boolProp "Include raw stdout/stderr in the response (default: false).")
    ] ["file"]

stackHoogleDef :: ToolDef
stackHoogleDef = ToolDef "stack_hoogle"
  "Search Haskell APIs with Hoogle. Set setup=true on first use to build haddock docs and \
  \generate a local Hoogle database that includes project code and all dependencies. \
  \Subsequent searches can omit setup for speed." $
  mkSchema
    [ ("query", strProp "Hoogle search query (e.g. a type signature or function name).")
    , ("setup", boolProp "Build haddock and generate local Hoogle DB before searching (slow on first run, enables project-local results).")
    , ("count", intProp "Maximum number of results (default: 10).")
    , ("flags", strProp "Additional flags.")
    ] []

------------------------------------------------------------------------
-- Implementations
------------------------------------------------------------------------

callStackExec :: Maybe FilePath -> Value -> IO ToolResult
callStackExec mcwd params = do
  let cmd     = getParamText "command" params
      cmdArgs = T.words (getParamText "args" params)
      inclW   = getParamBool "include_warnings" params
      inclO   = getParamBool "include_output" params
  if T.null cmd
    then pure $ mkToolError "command parameter is required"
    else do
      let args = ["exec", cmd, "--"] ++ cmdArgs
      so <- runStackRaw mcwd args
      let diags = parseGhcDiagnostics (soStderr so)
          mDiag = filteredDiagnosticsSummary inclW diags
      pure $ mkToolResultJSON $ object $
        [ "exit_code" .= soExitCode so
        ] ++ ["stdout" .= soStdout so | inclO]
          ++ maybe [] (\d -> ["diagnostics" .= d]) mDiag
          ++ ["stderr" .= soStderr so | inclO && soExitCode so /= 0 && null diags]

callStackGhci :: Maybe FilePath -> Value -> IO ToolResult
callStackGhci mcwd params = do
  let targets = T.words (getParamText "targets" params)
      noLoad  = case getParamMaybeBool "no_load" params of
                  Just b  -> b
                  Nothing -> False  -- default: load modules normally
      flags   = T.words (getParamText "flags" params)
      inclW   = getParamBool "include_warnings" params
      inclO   = getParamBool "include_output" params
      args    = ["ghci"] ++ targets ++ ["--no-load" | noLoad] ++ flags
  so <- runStackRaw mcwd args
  let diags   = parseGhcDiagnostics (soStderr so)
      depErrs = parseDepErrors (soStderr so)
      mDiag   = filteredDiagnosticsSummary inclW diags
  pure $ case soExitCode so of
    0 -> mkToolResultJSON $ object $
           ["stdout" .= soStdout so | inclO]
             ++ maybe [] (\d -> ["diagnostics" .= d]) mDiag
    _ -> mkCommandErrorFiltered inclW inclO args so diags depErrs mcwd

callStackGhc :: Maybe FilePath -> Value -> IO ToolResult
callStackGhc mcwd params = do
  let ghcArgs = T.words (getParamText "args" params)
      inclW   = getParamBool "include_warnings" params
      inclO   = getParamBool "include_output" params
  if null ghcArgs
    then pure $ mkToolError "args parameter is required"
    else do
      let args = ["ghc", "--"] ++ ghcArgs
      so <- runStackRaw mcwd args
      let diags = parseGhcDiagnostics (soStderr so)
          depErrs = parseDepErrors (soStderr so)
          mDiag = filteredDiagnosticsSummary inclW diags
      pure $ case soExitCode so of
        0 -> mkToolResultJSON $ object $
               ["stdout" .= soStdout so | inclO]
                 ++ maybe [] (\d -> ["diagnostics" .= d]) mDiag
        _ -> mkCommandErrorFiltered inclW inclO args so diags depErrs mcwd

callStackEval :: Maybe FilePath -> Value -> IO ToolResult
callStackEval mcwd params = do
  let expr  = getParamText "expression" params
      inclW = getParamBool "include_warnings" params
      inclO = getParamBool "include_output" params
  if T.null expr
    then pure $ mkToolError "expression parameter is required"
    else do
      let args = ["eval", expr]
      so <- runStackRaw mcwd args
      let diags = parseGhcDiagnostics (soStderr so)
          depErrs = parseDepErrors (soStderr so)
          mDiag = filteredDiagnosticsSummary inclW diags
      pure $ case soExitCode so of
        0 -> mkToolResultJSON $ object $
               [ "result"    .= T.strip (soStdout so)
               ] ++ maybe [] (\d -> ["diagnostics" .= d]) mDiag
        _ -> mkCommandErrorFiltered inclW inclO args so diags depErrs mcwd

callStackRunghc :: Maybe FilePath -> Value -> IO ToolResult
callStackRunghc mcwd params = do
  let file    = getParamText "file" params
      rArgs   = T.words (getParamText "args" params)
      inclW   = getParamBool "include_warnings" params
      inclO   = getParamBool "include_output" params
  if T.null file
    then pure $ mkToolError "file parameter is required"
    else do
      let args = ["runghc", "--", file] ++ rArgs
      so <- runStackRaw mcwd args
      let diags = parseGhcDiagnostics (soStderr so)
          depErrs = parseDepErrors (soStderr so)
          mDiag = filteredDiagnosticsSummary inclW diags
      pure $ case soExitCode so of
        0 -> mkToolResultJSON $ object $
               ["stdout" .= soStdout so | inclO]
                 ++ maybe [] (\d -> ["diagnostics" .= d]) mDiag
        _ -> mkCommandErrorFiltered inclW inclO args so diags depErrs mcwd

callStackScript :: Maybe FilePath -> Value -> IO ToolResult
callStackScript mcwd params = do
  let file     = getParamText "file" params
      resolver = getParamText "resolver" params
      rArgs    = T.words (getParamText "args" params)
      inclW    = getParamBool "include_warnings" params
      inclO    = getParamBool "include_output" params
  if T.null file
    then pure $ mkToolError "file parameter is required"
    else do
      let args = ["script", file]
              ++ ["--resolver" | not (T.null resolver)] ++ [resolver | not (T.null resolver)]
              ++ (if null rArgs then [] else "--" : rArgs)
      so <- runStackRaw mcwd args
      let diags = parseGhcDiagnostics (soStderr so)
          depErrs = parseDepErrors (soStderr so)
          mDiag = filteredDiagnosticsSummary inclW diags
      pure $ case soExitCode so of
        0 -> mkToolResultJSON $ object $
               ["stdout" .= soStdout so | inclO]
                 ++ maybe [] (\d -> ["diagnostics" .= d]) mDiag
        _ -> mkCommandErrorFiltered inclW inclO args so diags depErrs mcwd

callStackHoogle :: Maybe FilePath -> Value -> IO ToolResult
callStackHoogle mcwd params = do
  let query = getParamText "query" params
      setup = getParamBool "setup" params
      count = getParamInt "count" params
      flags = T.words (getParamText "flags" params)
  -- If setup requested, build haddock + generate local Hoogle DB first
  setupResult <- if setup
    then do
      haddockSo <- runStackRaw mcwd ["haddock", "--fast"]
      case soExitCode haddockSo of
        0 -> do
          genSo <- runStackRaw mcwd ["hoogle", "--", "generate", "--local"]
          case soExitCode genSo of
            0 -> pure $ Right "Hoogle database generated."
            _ -> pure $ Left $ mkCommandError ["hoogle", "--", "generate", "--local"] genSo
        _ -> pure $ Left $ mkCommandError ["haddock", "--fast"] haddockSo
    else pure $ Right ""
  case setupResult of
    Left err -> pure err
    Right setupMsg
      | T.null query -> do
          -- Setup-only: no search query provided
          if T.null setupMsg
            then pure $ mkToolError "query parameter is required (or set setup: true to build the Hoogle database)"
            else pure $ mkToolResultJSON $ object
                   [ "setup" .= setupMsg ]
      | otherwise -> do
          let countArg = case count of
                Just n  -> ["--count=" <> T.pack (show n)]
                Nothing -> []
              args = ["hoogle", "--"] ++ [query]
                  ++ countArg
                  ++ flags
          so <- runStackRaw mcwd args
          case soExitCode so of
            0 -> do
              let ls = filter (not . T.null) $ T.lines (soStdout so)
                  setupField = if T.null setupMsg then [] else ["setup" .= setupMsg]
              pure $ mkToolResultJSON $ object $
                [ "count"   .= length ls
                , "results" .= ls
                ] ++ setupField
            _ -> pure $ mkCommandError args so
