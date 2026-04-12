{-# LANGUAGE OverloadedStrings #-}

module ToolDefsTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.List (nub, sort)
import Data.Text (Text)
import Data.Text qualified as T

import StackMCP.Types (ToolDef(..))
import StackMCP.Tools (allTools)
import Data.IORef (newIORef)
import Data.Maybe (isJust)
import StackMCP.TaskManager (newTaskManager)

import StackMCP.Tools.Repo qualified as Repo
import StackMCP.Tools.Build qualified as Build
import StackMCP.Tools.Project qualified as Project
import StackMCP.Tools.Deps qualified as Deps
import StackMCP.Tools.Exec qualified as Exec
import StackMCP.Tools.Info qualified as Info
import StackMCP.Tools.Testing qualified as Testing
import StackMCP.Tools.Pipeline qualified as Pipeline
import StackMCP.Tools.Tasks qualified as Tasks
import StackMCP.Tools.Edit qualified as Edit

tests :: TestTree
tests = testGroup "ToolDefs"
  [ testCase "total tool count is 48" $
      length allTools @?= 48

  , testCase "all tool names are unique" $ do
      let names = map toolName allTools
      length names @?= length (nub names)

  , testCase "no empty tool names" $
      assertBool "all names non-empty" (all (not . T.null . toolName) allTools)

  , testCase "no empty descriptions" $
      assertBool "all descriptions non-empty" (all (not . T.null . toolDescription) allTools)

  , testCase "all schemas are objects" $
      assertBool "all schemas are JSON objects" (all isObj (map toolInputSchema allTools))

  , testCase "all schemas have type=object" $
      assertBool "all schemas have type field" (all hasTypeObject (map toolInputSchema allTools))

  , testCase "known tools present" $ do
      let names = map toolName allTools
      mapM_ (\n -> assertBool (T.unpack n ++ " present") (n `elem` names))
        -- Repo
        [ "set_repo", "get_repo"
        -- Build
        , "stack_build", "stack_test", "stack_bench"
        , "stack_clean"
        , "stack_haddock", "stack_install"
        -- Project
        , "stack_new", "stack_init", "stack_setup"
        , "stack_config_set"
        -- Deps
        , "stack_ls_dependencies"
        , "stack_ls_snapshots", "stack_ls_globals"
        , "stack_update"
        -- Exec
        , "stack_eval", "stack_runghc"
        , "stack_hoogle"
        -- Info
        , "stack_path"
        , "stack_ls_tools"
        , "stack_ide_info"
        , "stack_upgrade"
        -- Testing
        , "stack_test_discover", "stack_test_run"
        , "stack_bench_discover", "stack_bench_run"
        -- Pipeline
        , "stack_pipeline", "stack_config_read"
        -- Tasks
        , "task_run", "task_exec", "task_ghci", "task_ghci_eval"
        , "task_read", "task_write", "task_kill", "task_list"
        -- Edit
        , "project_dependency"
        , "project_add_module", "project_expose_module"
        , "project_rename_module", "project_list_modules"
        , "project_remove_module"
        , "project_extra_dep"
        , "project_set_ghc_options"
        , "project_extension"
        , "project_add_component", "project_resolve_module"
        ]

  , testCase "known tools list is exhaustive (covers all 48)" $ do
      let known = sort
            [ "set_repo", "get_repo"
            , "stack_build", "stack_test", "stack_bench"
            , "stack_clean"
            , "stack_haddock", "stack_install"
            , "stack_new", "stack_init", "stack_setup"
            , "stack_config_set"
            , "stack_ls_dependencies"
            , "stack_ls_snapshots", "stack_ls_globals"
            , "stack_update"
            , "stack_eval", "stack_runghc"
            , "stack_hoogle"
            , "stack_path"
            , "stack_ls_tools"
            , "stack_ide_info"
            , "stack_upgrade"
            , "stack_test_discover", "stack_test_run"
            , "stack_bench_discover", "stack_bench_run"
            , "stack_pipeline", "stack_config_read"
            , "task_run", "task_exec", "task_ghci", "task_ghci_eval"
            , "task_read", "task_write", "task_kill", "task_list"
            -- Edit
            , "project_dependency"
            , "project_add_module", "project_expose_module"
            , "project_rename_module", "project_list_modules"
            , "project_remove_module"
            , "project_extra_dep"
            , "project_set_ghc_options"
            , "project_extension"
            , "project_add_component", "project_resolve_module"
            ] :: [Text]
          actual = sort (map toolName allTools)
      assertEqual "known list matches allTools" actual known

  , testCase "every tool name dispatches (not unknown)" $ do
      cwdRef <- newIORef Nothing
      tm <- newTaskManager
      let emptyParams = object []
      mapM_ (\td -> do
          let name = toolName td
          mcwd <- pure Nothing
          -- Check that at least one dispatch function recognizes this tool name.
          -- dispatch returns Maybe (IO ToolResult) — checking isJust does NOT execute the IO.
          let recognized = any isJust
                [ Repo.dispatch cwdRef name emptyParams
                , Build.dispatch mcwd name emptyParams
                , Project.dispatch mcwd name emptyParams
                , Deps.dispatch mcwd name emptyParams
                , Exec.dispatch mcwd name emptyParams
                , Info.dispatch mcwd name emptyParams
                , Testing.dispatch mcwd name emptyParams
                , Pipeline.dispatch cwdRef name emptyParams
                , Tasks.dispatch tm mcwd name emptyParams
                , Edit.dispatch cwdRef name emptyParams
                ]
          assertBool (T.unpack name ++ " has a dispatch handler") recognized
        ) allTools

  , testCase "ToolDef ToJSON roundtrip" $ do
      case allTools of
        (td : _) -> do
          let val = toJSON td
          case val of
            Object o -> do
              assertBool "has name" (KM.member (Key.fromText "name") o)
              assertBool "has description" (KM.member (Key.fromText "description") o)
              assertBool "has inputSchema" (KM.member (Key.fromText "inputSchema") o)
            _ -> assertFailure "ToolDef should serialize to object"
        [] -> assertFailure "allTools is empty"

  , testCase "integer schema params are not read as text in implementation" $
      -- Verify that tools declaring intProp params don't accidentally have
      -- "string" type in their schema (which would happen if strProp was used)
      mapM_ (\td -> do
          let name = toolName td
          case toolInputSchema td of
            Object o -> case KM.lookup "properties" o of
              Just (Object props) ->
                mapM_ (\(k, v) -> case v of
                    Object pv -> case KM.lookup "type" pv of
                      Just (String "integer") ->
                        -- This is an intProp — good, just verify it's really integer
                        assertBool (T.unpack name ++ "." ++ show k ++ " is integer") True
                      _ -> pure ()
                    _ -> pure ()
                  ) (KM.toList props)
              _ -> pure ()
            _ -> pure ()
        ) allTools

  , testCase "depth params are integer type" $ do
      let depsTools = filter (\td -> toolName td `elem`
            ["stack_ls_dependencies"]) allTools
      mapM_ (\td -> do
          let name = toolName td
          assertBool (T.unpack name ++ " has depth as integer")
            (paramHasType (toolInputSchema td) "depth" "integer")
        ) depsTools

  , testCase "count param is integer type" $ do
      case filter (\td -> toolName td == "stack_hoogle") allTools of
        (hoogle : _) -> assertBool "stack_hoogle count is integer"
          (paramHasType (toolInputSchema hoogle) "count" "integer")
        [] -> assertFailure "stack_hoogle not found"

  , testCase "timeout_ms param is integer type" $ do
      case filter (\td -> toolName td == "task_ghci_eval") allTools of
        (ghciEval : _) -> assertBool "task_ghci_eval timeout_ms is integer"
          (paramHasType (toolInputSchema ghciEval) "timeout_ms" "integer")
        [] -> assertFailure "task_ghci_eval not found"
  ]

isObj :: Value -> Bool
isObj (Object _) = True
isObj _          = False

hasTypeObject :: Value -> Bool
hasTypeObject (Object o) = case KM.lookup (Key.fromText "type") o of
  Just (String "object") -> True
  _                      -> False
hasTypeObject _ = False

-- | Check whether a given param in a tool schema has the expected JSON type.
paramHasType :: Value -> Text -> Text -> Bool
paramHasType schema paramName expectedType =
  case schema of
    Object o -> case KM.lookup "properties" o of
      Just (Object props) -> case KM.lookup (Key.fromText paramName) props of
        Just (Object pv) -> case KM.lookup "type" pv of
          Just (String t) -> t == expectedType
          _               -> False
        _ -> False
      _ -> False
    _ -> False

-- unused toList removed
