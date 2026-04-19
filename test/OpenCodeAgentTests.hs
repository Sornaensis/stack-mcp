{-# LANGUAGE OverloadedStrings #-}

module OpenCodeAgentTests (tests) where

import Control.Monad (forM_)
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import StackMCP.Tools (allTools)
import StackMCP.Types (ToolDef(..))

tests :: TestTree
tests = testGroup "OpenCode agents"
  [ testCase "all stack_mcp permissions map to real server tools" $ do
      manifests <- loadAgentFiles
      let validRefs = map (("stack_mcp/" <>) . toolName) allTools
      forM_ manifests $ \(path, refs) ->
        forM_ refs $ \ref ->
          assertBool (path <> " references unknown tool: " <> T.unpack ref)
            (ref `elem` validRefs)
  ]

loadAgentFiles :: IO [(FilePath, [Text])]
loadAgentFiles = do
  let agentDir = "opencode-agents"
  files <- listDirectory agentDir
  let manifestFiles = [agentDir </> f | f <- files, ".md" `T.isSuffixOf` T.pack f]
  mapM loadManifest manifestFiles

loadManifest :: FilePath -> IO (FilePath, [Text])
loadManifest path = do
  content <- readFile path
  let refs = [ key
             | line <- T.lines (T.pack content)
             , let stripped = T.strip line
             , ": true" `T.isSuffixOf` stripped
             , let key = T.dropEnd 6 stripped
             , "stack_mcp/" `T.isPrefixOf` key
             ]
  pure (path, refs)
