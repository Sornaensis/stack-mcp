{-# LANGUAGE OverloadedStrings #-}

module StackMCP.Tools.Project
  ( tools
  , dispatch
  ) where

import Data.Text qualified as T
import StackMCP.Tools.Common

tools :: [ToolDef]
tools =
  [ stackNewDef
  , stackInitDef
  , stackSetupDef
  , stackConfigSetDef
  ]

dispatch :: Maybe FilePath -> Text -> Value -> Maybe (IO ToolResult)
dispatch mcwd name params = case name of
  "stack_new"        -> Just $ callStackNew mcwd params
  "stack_init"       -> Just $ callStackInit mcwd params
  "stack_setup"      -> Just $ callStackSetup mcwd params
  "stack_config_set" -> Just $ callStackConfigSet mcwd params
  _                  -> Nothing

------------------------------------------------------------------------
-- Definitions
------------------------------------------------------------------------

stackNewDef :: ToolDef
stackNewDef = ToolDef "stack_new"
  "Create a new Stack project from a template." $
  mkSchema
    [ ("name", strProp "Project name (required).")
    , ("template", strProp "Template name (e.g. new-template, simple, rio). Or a URL/local path.")
    , ("resolver", strProp "Snapshot resolver to use (e.g. lts-24.2).")
    ] ["name"]

stackInitDef :: ToolDef
stackInitDef = ToolDef "stack_init"
  "Create stack.yaml from existing Cabal or Hpack package specifications." $
  mkSchema
    [ ("resolver", strProp "Snapshot resolver to use.")
    , ("force", boolProp "Force initialization even if stack.yaml exists (--force).")
    ] []

stackSetupDef :: ToolDef
stackSetupDef = ToolDef "stack_setup"
  "Download and install the appropriate GHC for the current project." $
  mkSchema
    [ ("ghc_version", strProp "Specific GHC version to install (optional, defaults to resolver's GHC).")
    ] []

stackConfigSetDef :: ToolDef
stackConfigSetDef = ToolDef "stack_config_set"
  "Set a Stack configuration value in stack.yaml or global config." $
  mkSchema
    [ ("key", strProp "Configuration key (e.g. resolver, system-ghc).")
    , ("value", strProp "Value to set.")
    , ("global", boolProp "Set in global config instead of project config (--global).")
    ] ["key", "value"]

------------------------------------------------------------------------
-- Implementations
------------------------------------------------------------------------

callStackNew :: Maybe FilePath -> Value -> IO ToolResult
callStackNew mcwd params = do
  let name     = getParamText "name" params
      template = getParamText "template" params
      resolver = getParamText "resolver" params
  if T.null name
    then pure $ mkToolError "name parameter is required"
    else do
      let args = ["new", name]
              ++ [template | not (T.null template)]
              ++ ["--resolver" | not (T.null resolver)] ++ [resolver | not (T.null resolver)]
      so <- runStackRaw mcwd args
      pure $ case soExitCode so of
        0 -> mkToolResultJSON $ object
          [ "project" .= name, "output" .= soStdout so
          ]
        _ -> mkCommandError args so

callStackInit :: Maybe FilePath -> Value -> IO ToolResult
callStackInit mcwd params = do
  let resolver = getParamText "resolver" params
      force    = getParamBool "force" params
      args = ["init"]
          ++ ["--resolver" | not (T.null resolver)] ++ [resolver | not (T.null resolver)]
          ++ ["--force" | force]
  so <- runStackRaw mcwd args
  pure $ case soExitCode so of
    0 -> mkToolResultJSON $ object ["output" .= soStdout so]
    _ -> mkCommandError args so

callStackSetup :: Maybe FilePath -> Value -> IO ToolResult
callStackSetup mcwd params = do
  let ver = getParamText "ghc_version" params
      args = ["setup"] ++ [ver | not (T.null ver)]
  so <- runStackRaw mcwd args
  pure $ case soExitCode so of
    0 -> mkToolResultJSON $ object
      [ "output" .= soStdout so
      ]
    _ -> mkCommandError args so

callStackConfigSet :: Maybe FilePath -> Value -> IO ToolResult
callStackConfigSet mcwd params = do
  let key    = getParamText "key" params
      val    = getParamText "value" params
      global = getParamBool "global" params
  if T.null key || T.null val
    then pure $ mkToolError "key and value parameters are required"
    else do
      let args = ["config", "set"] ++ ["--global" | global] ++ [key, val]
      so <- runStackRaw mcwd args
      pure $ case soExitCode so of
        0 -> mkToolResultJSON $ object
          ["key" .= key, "value" .= val]
        _ -> mkCommandError args so
