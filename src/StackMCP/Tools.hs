{-# LANGUAGE OverloadedStrings #-}

module StackMCP.Tools
  ( allTools
  , toolPages
  , callTool
  ) where

import Data.IORef
import Data.Text (Text)
import Data.Aeson (Value)
import StackMCP.Types (ToolDef, ToolResult, mkToolError)
import StackMCP.TaskManager (TaskManager)

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

-- | Tools grouped by category for paginated listing.
toolPages :: [(Text, [ToolDef])]
toolPages =
  [ ("repo",     Repo.tools)
  , ("build",    Build.tools)
  , ("project",  Project.tools)
  , ("deps",     Deps.tools)
  , ("exec",     Exec.tools)
  , ("info",     Info.tools)
  , ("testing",  Testing.tools)
  , ("pipeline", Pipeline.tools)
  , ("tasks",    Tasks.tools)
  , ("edit",     Edit.tools)
  ]

-- | All tools from every submodule.
allTools :: [ToolDef]
allTools = concatMap snd toolPages

-- | Dispatch a tool call by name. Tries each submodule's dispatch in order.
callTool :: IORef (Maybe FilePath) -> TaskManager -> Text -> Value -> IO ToolResult
callTool cwdRef tm name params = do
  mcwd <- readIORef cwdRef
  let dispatchers =
        [ Repo.dispatch cwdRef name params
        , Build.dispatch mcwd name params
        , Project.dispatch mcwd name params
        , Deps.dispatch mcwd name params
        , Exec.dispatch mcwd name params
        , Info.dispatch mcwd name params
        , Testing.dispatch mcwd name params
        , Pipeline.dispatch cwdRef name params
        , Tasks.dispatch tm mcwd name params
        , Edit.dispatch cwdRef name params
        ]
  case firstJust dispatchers of
    Just action -> action
    Nothing     -> pure $ mkToolError ("Unknown tool: " <> name)

firstJust :: [Maybe a] -> Maybe a
firstJust []            = Nothing
firstJust (Just x : _)  = Just x
firstJust (Nothing : xs) = firstJust xs

