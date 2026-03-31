{-# LANGUAGE OverloadedStrings #-}

module StackMCP.Prompts
  ( allPrompts
  , getPrompt
  ) where

import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Data.Text qualified as T
import StackMCP.Types

allPrompts :: [PromptDef]
allPrompts =
  [ PromptDef "debug-build-failure"
      (Just "Analyze a Stack build failure and suggest fixes")
      [ PromptArgument "error_output" (Just "The build error output to analyze") True ]
  , PromptDef "add-dependency"
      (Just "Guide through adding a new package dependency")
      [ PromptArgument "package" (Just "Package name to add") True
      , PromptArgument "version" (Just "Optional version constraint") False
      ]
  , PromptDef "setup-new-project"
      (Just "Guide through creating a new Haskell Stack project")
      [ PromptArgument "name" (Just "Project name") True
      , PromptArgument "template" (Just "Template to use (e.g. new-template, simple)") False
      ]
  , PromptDef "analyze-dependencies"
      (Just "Analyze project dependencies for issues, outdated packages, and suggestions")
      []
  , PromptDef "configure-ghc-options"
      (Just "Help configure GHC options for the project")
      [ PromptArgument "goal" (Just "What you're trying to achieve (e.g. performance, warnings, profiling)") True ]
  ]

getPrompt :: Text -> Value -> Either Text [PromptMessage]
getPrompt name params = case name of
  "debug-build-failure"  -> Right $ debugBuildFailure params
  "add-dependency"       -> Right $ addDependency params
  "setup-new-project"    -> Right $ setupNewProject params
  "analyze-dependencies" -> Right analyzeDependencies
  "configure-ghc-options" -> Right $ configureGhcOptions params
  _                      -> Left ("Unknown prompt: " <> name)

------------------------------------------------------------------------
-- Prompt implementations
------------------------------------------------------------------------

debugBuildFailure :: Value -> [PromptMessage]
debugBuildFailure params =
  let err = getArg "error_output" params
  in [ PromptMessage "user" $ textContent $
         "I'm getting a build error in my Haskell Stack project. Please analyze this error and suggest fixes.\n\n"
         <> "Build error output:\n```\n" <> err <> "\n```\n\n"
         <> "Please:\n"
         <> "1. Identify the root cause of the error\n"
         <> "2. Suggest specific fixes\n"
         <> "3. If it's a dependency issue, suggest version constraints\n"
         <> "4. If it's a code issue, explain what needs to change\n\n"
         <> "Use the provided error output as the primary source. If one targeted tool call is absolutely necessary, make at most one of stack_build, stack_path, or stack_ls_dependencies, then answer. Do not keep investigating after that."
     ]

addDependency :: Value -> [PromptMessage]
addDependency params =
  let pkg = getArg "package" params
      ver = getArg "version" params
  in [ PromptMessage "user" $ textContent $
         "I want to add the package '" <> pkg <> "'"
         <> (if T.null ver then "" else " with version constraint '" <> ver <> "'")
         <> " to my Haskell Stack project.\n\n"
         <> "Please:\n"
         <> "1. Check if the package exists using stack_list\n"
         <> "2. Add it to the dependencies in package.yaml\n"
         <> "3. If needed, add it to extra-deps in stack.yaml\n"
         <> "4. Run stack_build to verify it compiles\n"
         <> "5. Show a usage example for the package"
     ]

setupNewProject :: Value -> [PromptMessage]
setupNewProject params =
  let name = getArg "name" params
      tmpl = getArg "template" params
  in [ PromptMessage "user" $ textContent $
         "I want to create a new Haskell Stack project called '" <> name <> "'"
         <> (if T.null tmpl then "" else " using the '" <> tmpl <> "' template")
         <> ".\n\n"
         <> "Please:\n"
         <> "1. Use stack_new to create the project\n"
         <> "2. Use set_repo to set the working directory to the new project\n"
         <> "3. Run stack_setup to ensure GHC is installed\n"
         <> "4. Run stack_build to verify the project compiles\n"
         <> "5. Show the project structure and explain the key files"
     ]

analyzeDependencies :: [PromptMessage]
analyzeDependencies =
  [ PromptMessage "user" $ textContent $
      "Please analyze the dependencies of my Haskell Stack project.\n\n"
      <> "Steps:\n"
      <> "1. Use stack_ls_dependencies_json to get the full dependency list\n"
      <> "2. Use stack_ls_dependencies_tree to see the dependency tree\n"
      <> "3. Check for outdated packages using stack_list\n"
      <> "4. Identify any potential issues (version conflicts, redundant deps)\n"
      <> "5. Suggest improvements or updates"
  ]

configureGhcOptions :: Value -> [PromptMessage]
configureGhcOptions params =
  let goal = getArg "goal" params
  in [ PromptMessage "user" $ textContent $
         "I want to configure GHC options for my Haskell Stack project. My goal: " <> goal <> "\n\n"
         <> "Please:\n"
         <> "1. Check current project configuration using stack_path and stack_config_read\n"
         <> "2. Recommend appropriate GHC options for my goal\n"
         <> "3. Show how to set them in package.yaml and/or stack.yaml\n"
         <> "4. Explain any trade-offs\n"
         <> "5. Test the build with stack_build to verify the options work"
     ]

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

getArg :: Text -> Value -> Text
getArg key val = case val of
  Object o -> case KM.lookup (fromText key) o of
    Just (String s) -> s
    _               -> ""
  _ -> ""
