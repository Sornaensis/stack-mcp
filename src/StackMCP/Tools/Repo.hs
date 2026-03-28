{-# LANGUAGE OverloadedStrings #-}

module StackMCP.Tools.Repo
  ( tools
  , dispatch
  ) where

import Data.IORef
import Data.Text qualified as T
import System.Directory (doesDirectoryExist, doesFileExist, makeAbsolute, getCurrentDirectory)
import System.FilePath ((</>))
import StackMCP.Tools.Common

tools :: [ToolDef]
tools =
  [ setRepoDef
  , getRepoDef
  ]

dispatch :: IORef (Maybe FilePath) -> Text -> Value -> Maybe (IO ToolResult)
dispatch cwdRef name params = case name of
  "set_repo" -> Just $ callSetRepo cwdRef params
  "get_repo" -> Just $ callGetRepo cwdRef
  _          -> Nothing

------------------------------------------------------------------------
-- Definitions
------------------------------------------------------------------------

setRepoDef :: ToolDef
setRepoDef = ToolDef "set_repo"
  "Set the working directory for all subsequent Stack commands. Must be called before using any Stack tool. Validates the directory exists and reports whether stack.yaml is found." $
  mkSchema
    [ ("path", strProp "Absolute path to a Haskell Stack project directory.")
    ] ["path"]

getRepoDef :: ToolDef
getRepoDef = ToolDef "get_repo"
  "Get the current working directory used for Stack commands. Returns the explicitly-set path or the server's working directory if none is set." $
  mkSchema [] []

------------------------------------------------------------------------
-- Implementations
------------------------------------------------------------------------

callSetRepo :: IORef (Maybe FilePath) -> Value -> IO ToolResult
callSetRepo cwdRef params = do
  let rawPath = T.unpack (getParamText "path" params)
  if null rawPath
    then pure $ mkToolError "path parameter is required"
    else do
      absPath <- makeAbsolute rawPath
      exists  <- doesDirectoryExist absPath
      if not exists
        then pure $ mkToolError ("Directory does not exist: " <> T.pack absPath)
        else do
          let stackYaml = absPath </> "stack.yaml"
          hasStack <- doesFileExist stackYaml
          writeIORef cwdRef (Just absPath)
          let hint = if hasStack then [] else ["hint" .= ("No stack.yaml found. Run stack_init to create one, or stack_new to scaffold a new project." :: Text)]
          pure $ mkToolResultJSON $ object $
            [ "path"       .= T.pack absPath
            , "stack_yaml" .= hasStack
            ] ++ hint

callGetRepo :: IORef (Maybe FilePath) -> IO ToolResult
callGetRepo cwdRef = do
  mcwd <- readIORef cwdRef
  case mcwd of
    Nothing  -> do
      cwd <- getCurrentDirectory
      pure $ mkToolResultJSON $ object
        [ "explicit" .= False
        , "path"     .= T.pack cwd
        ]
    Just dir -> pure $ mkToolResultJSON $ object
        [ "explicit" .= True
        , "path"     .= T.pack dir
        ]
