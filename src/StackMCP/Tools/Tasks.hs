{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StackMCP.Tools.Tasks
  ( tools
  , dispatch
  ) where

import Data.Text qualified as T
import StackMCP.Tools.Common
import StackMCP.Tools.Parse (parseGhcDiagnostics, diagnosticsSummary)
import StackMCP.TaskManager

tools :: [ToolDef]
tools =
  [ taskRunDef
  , taskExecDef
  , taskGhciDef
  , taskGhciEvalDef
  , taskReadDef
  , taskWriteDef
  , taskKillDef
  , taskListDef
  ]

dispatch :: TaskManager -> Maybe FilePath -> Text -> Value -> Maybe (IO ToolResult)
dispatch tm mcwd name params = case name of
  "task_run"  -> Just $ callTaskRun  tm mcwd params
  "task_exec" -> Just $ callTaskExec tm mcwd params
  "task_ghci" -> Just $ callTaskGhci tm mcwd params
  "task_ghci_eval" -> Just $ callTaskGhciEval tm params
  "task_read" -> Just $ callTaskRead tm params
  "task_write"-> Just $ callTaskWrite tm params
  "task_kill" -> Just $ callTaskKill tm params
  "task_list" -> Just $ callTaskList tm
  _           -> Nothing

------------------------------------------------------------------------
-- Definitions
------------------------------------------------------------------------

taskRunDef :: ToolDef
taskRunDef = ToolDef "task_run"
  "Spawn a background 'stack run' process. Returns a task ID for monitoring. \
  \Use this for long-running executables (servers, daemons). For one-shot runs, use stack_run instead (via @stack-build)." $
  mkSchema
    [ ("args", strProp "Arguments to pass to 'stack run' (e.g. target name, -- followed by program args).")
    ] []

taskExecDef :: ToolDef
taskExecDef = ToolDef "task_exec"
  "Spawn a background 'stack exec' process. Returns a task ID for monitoring. \
  \Use this for long-running or interactive commands. For one-shot commands, use stack_exec instead (via @stack-exec)." $
  mkSchema
    [ ("command", strProp "The command to execute in the Stack environment (required).")
    , ("args", strProp "Space-separated arguments for the command (passed after --).")
    ] ["command"]

taskGhciDef :: ToolDef
taskGhciDef = ToolDef "task_ghci"
  "Spawn an interactive 'stack ghci' session. Blocks until GHCi is fully loaded and ready. \
  \Use task_ghci_eval to evaluate expressions. For non-interactive GHCi info, use stack_ghci instead (via @stack-exec)." $
  mkSchema
    [ ("targets", strProp "Targets to load in GHCi (e.g. package name or component).")
    , ("args", strProp "Additional arguments (e.g. \"--no-load\").")
    ] []

taskGhciEvalDef :: ToolDef
taskGhciEvalDef = ToolDef "task_ghci_eval"
  "Evaluate an expression in a running GHCi session. Uses sentinel-based completion detection for reliable results. Returns the clean output with prompt noise stripped." $
  mkSchema
    [ ("task_id", strProp "The GHCi task ID (required).")
    , ("expression", strProp "Haskell expression or GHCi command to evaluate (required, e.g. \":t map\", \"1 + 1\", \":info Maybe\").")
    , ("timeout_ms", intProp "Maximum time to wait for a result in milliseconds (default: 5000).")
    ] ["task_id", "expression"]

taskReadDef :: ToolDef
taskReadDef = ToolDef "task_read"
  "Read accumulated stdout and stderr from a background task. Drains the buffers. Includes GHC diagnostics when the task has exited with errors." $
  mkSchema
    [ ("task_id", strProp "The task ID returned by task_run/task_exec/task_ghci (required).")
    ] ["task_id"]

taskWriteDef :: ToolDef
taskWriteDef = ToolDef "task_write"
  "Write text to a background task's stdin." $
  mkSchema
    [ ("task_id", strProp "The task ID (required).")
    , ("input", strProp "Text to send to the task's stdin (required).")
    ] ["task_id", "input"]

taskKillDef :: ToolDef
taskKillDef = ToolDef "task_kill"
  "Terminate a background task." $
  mkSchema
    [ ("task_id", strProp "The task ID to terminate (required).")
    ] ["task_id"]

taskListDef :: ToolDef
taskListDef = ToolDef "task_list"
  "List all background tasks with their current status." $
  mkSchema [] []

------------------------------------------------------------------------
-- Implementations
------------------------------------------------------------------------

callTaskRun :: TaskManager -> Maybe FilePath -> Value -> IO ToolResult
callTaskRun tm mcwd params = do
  let args = T.words (getParamText "args" params)
  result <- spawnTask tm mcwd (TaskRun args)
  pure $ case result of
    Left (SpawnFailed msg) -> mkStructuredError "spawn_failed"
      ("Failed to start 'stack run': " <> msg)
      ["stack", "run"] ["Check that stack is installed and on PATH", "Verify the project builds successfully"]
    Right tid -> mkToolResultJSON $ object
      [ "task_id" .= tid
      , "command" .= commandToDisplay (TaskRun args)
      , "status"  .= ("running" :: Text)
      ]

callTaskExec :: TaskManager -> Maybe FilePath -> Value -> IO ToolResult
callTaskExec tm mcwd params = do
  let cmd  = getParamText "command" params
      args = T.words (getParamText "args" params)
  if T.null cmd
    then pure $ mkToolError "command parameter is required"
    else do
      result <- spawnTask tm mcwd (TaskExec cmd args)
      pure $ case result of
        Left (SpawnFailed msg) -> mkStructuredError "spawn_failed"
          ("Failed to start 'stack exec " <> cmd <> "': " <> msg)
          ["stack", "exec", cmd] ["Check that stack is installed and on PATH", "Verify the executable exists"]
        Right tid -> mkToolResultJSON $ object
          [ "task_id" .= tid
          , "command" .= commandToDisplay (TaskExec cmd args)
          , "status"  .= ("running" :: Text)
          ]

callTaskGhci :: TaskManager -> Maybe FilePath -> Value -> IO ToolResult
callTaskGhci tm mcwd params = do
  let targets = T.words (getParamText "targets" params)
      extra   = T.words (getParamText "args" params)
      args    = targets ++ extra
  result <- spawnTask tm mcwd (TaskGhci args)
  case result of
    Left (SpawnFailed msg) -> pure $ mkStructuredError "spawn_failed"
      ("Failed to start 'stack ghci': " <> msg)
      ["stack", "ghci"] ["Check that stack is installed and on PATH", "Ensure GHC is set up via 'stack setup'"]
    Right tid -> do
      -- Configure deterministic prompts and wait for GHCi to be ready.
      -- The :set commands queue in the pipe; GHCi processes them after loading.
      let setupCmd = ":set prompt \""  <> ghciPromptMarker <> "\"\n"
                  <> ":set prompt-cont \"" <> ghciContMarker  <> "\"\n"
      -- Wait up to 120s for GHCi to load modules and show our prompt
      initResult <- writeAndRead tm tid setupCmd ghciPromptMarker 200000 600
      case initResult of
        Left err -> pure $ mkStructuredError "ghci_init_failed"
          ("GHCi session failed to initialize: " <> err)
          ["stack", "ghci"] ["Check that the project builds successfully"]
        Right (_out, err, status) -> case status of
          Exited n -> do
            let diags = parseGhcDiagnostics err
                diagField = if null diags then [] else ["diagnostics" .= diagnosticsSummary diags]
            pure $ mkToolErrorJSON $ object $
              [ "error_type" .= ("ghci_init_failed" :: Text)
              , "task_id"    .= tid
              , "exit_code"  .= n
              , "stderr"     .= err
              ] ++ diagField
          Running -> pure $ mkToolResultJSON $ object
            [ "task_id" .= tid
            , "command" .= commandToDisplay (TaskGhci args)
            , "status"  .= ("ready" :: Text)
            ]

callTaskGhciEval :: TaskManager -> Value -> IO ToolResult
callTaskGhciEval tm params = do
  let tid    = getParamText "task_id" params
      expr   = getParamText "expression" params
      timeMs = maybe 5000 id (getParamInt "timeout_ms" params)
  if T.null tid
    then pure $ mkToolError "task_id parameter is required"
    else if T.null expr
    then pure $ mkToolError "expression parameter is required"
    else do
      let delayUs    = 50000  -- 50ms per poll
          maxRetries = max 1 ((timeMs * 1000) `div` delayUs)
          sentinelCmd = "putStrLn \"" <> ghciSentinelTag <> "\""
          fullInput   = expr <> "\n" <> sentinelCmd <> "\n"
      result <- writeAndRead tm tid fullInput ghciSentinelTag delayUs maxRetries
      case result of
        Left e -> pure $ mkToolError e
        Right (rawOut, err, status) ->
          let cleaned  = extractGhciResult rawOut
              timedOut = not (ghciSentinelTag `T.isInfixOf` rawOut) && status == Running
              statusText = case status of
                Running  | timedOut -> "timeout" :: Text
                         | otherwise -> "ok"
                Exited 0  -> "exited_success"
                Exited _  -> "exited_error"
          in pure $ mkToolResultJSON $ object $
               [ "task_id"    .= tid
               , "expression" .= expr
               , "result"     .= cleaned
               , "status"     .= statusText
               ] ++ ["stderr" .= err | not (T.null err)]
                 ++ ["timed_out" .= True | timedOut]

------------------------------------------------------------------------
-- GHCi protocol
------------------------------------------------------------------------

-- | Prompt marker set via @:set prompt@ at session init.
--   Deliberately unusual to avoid collisions with normal output.
ghciPromptMarker :: Text
ghciPromptMarker = "~#~GHCI~#~ "

-- | Continuation prompt marker set via @:set prompt-cont@.
ghciContMarker :: Text
ghciContMarker = "~#~CONT~#~ "

-- | Sentinel tag printed via @putStrLn@ after each eval expression.
--   Its appearance in stdout signals that GHCi has finished processing.
ghciSentinelTag :: Text
ghciSentinelTag = "~#~EVAL_DONE~#~"

-- | Extract the clean result from GHCi stdout that contains prompt markers
--   and a sentinel tag.  Takes all lines before the sentinel, strips known
--   prompt prefixes, and trims whitespace.
extractGhciResult :: Text -> Text
extractGhciResult raw =
  let lns = T.lines raw
      -- Everything before the line containing the sentinel
      beforeSentinel = takeWhile (not . (ghciSentinelTag `T.isInfixOf`)) lns
      -- Strip our known prompt/continuation prefixes
      cleaned = map stripMarkers beforeSentinel
  in T.strip (T.unlines cleaned)
  where
    stripMarkers = stripPfx ghciContMarker . stripPfx ghciPromptMarker
    stripPfx pfx line
      | pfx `T.isPrefixOf` line = T.drop (T.length pfx) line
      | otherwise               = line

callTaskRead :: TaskManager -> Value -> IO ToolResult
callTaskRead tm params = do
  let tid = getParamText "task_id" params
  if T.null tid
    then pure $ mkToolError "task_id parameter is required"
    else do
      mOutput <- readTaskOutput tm tid
      case mOutput of
        Nothing -> pure $ mkToolError ("Task not found: " <> tid)
        Just (out, err, status) ->
          let statusText = case status of
                Running   -> "running" :: Text
                Exited 0  -> "exited_success"
                Exited _  -> "exited_error"
              exitCode = case status of
                Running  -> Nothing
                Exited n -> Just n
              -- Parse GHC diagnostics from stderr when the task has failed
              diagsObj = case status of
                Exited n | n /= 0 && not (T.null err) ->
                  let diags = parseGhcDiagnostics err
                  in if null diags then [] else ["diagnostics" .= diagnosticsSummary diags]
                _ -> []
          in pure $ mkToolResultJSON $ object $
               [ "task_id" .= tid
               , "status"  .= statusText
               , "stdout"  .= out
               , "stderr"  .= err
               ] ++ maybe [] (\c -> ["exit_code" .= c]) exitCode
                 ++ diagsObj

callTaskWrite :: TaskManager -> Value -> IO ToolResult
callTaskWrite tm params = do
  let tid   = getParamText "task_id" params
      input = getParamText "input" params
  if T.null tid
    then pure $ mkToolError "task_id parameter is required"
    else if T.null input
    then pure $ mkToolError "input parameter is required"
    else do
      mErr <- writeTaskStdin tm tid input
      pure $ case mErr of
        Just e  -> mkToolError e
        Nothing -> mkToolResultJSON $ object
          [ "task_id" .= tid
          , "written" .= T.length input
          ]

callTaskKill :: TaskManager -> Value -> IO ToolResult
callTaskKill tm params = do
  let tid = getParamText "task_id" params
  if T.null tid
    then pure $ mkToolError "task_id parameter is required"
    else do
      mErr <- killTask tm tid
      pure $ case mErr of
        Just e  -> mkToolError e
        Nothing -> mkToolResultJSON $ object
          [ "task_id"    .= tid
          , "terminated" .= True
          ]

callTaskList :: TaskManager -> IO ToolResult
callTaskList tm = do
  infos <- listTasks tm
  let toObj i = object
        [ "task_id" .= tiId i
        , "command" .= tiCommand i
        , "status"  .= statusText (tiStatus i)
        ]
      statusText Running   = "running" :: Text
      statusText (Exited 0) = "exited_success"
      statusText (Exited _) = "exited_error"
  pure $ mkToolResultJSON $ object
    [ "count" .= length infos
    , "tasks" .= map toObj infos
    ]
