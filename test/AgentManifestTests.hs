{-# LANGUAGE OverloadedStrings #-}

module AgentManifestTests (tests) where

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
tests = testGroup "Agent manifests"
  [ testCase "all MCP tool references use OpenCode naming" $ do
      manifests <- loadAgentManifests
      forM_ manifests $ \(path, refs) ->
        forM_ refs $ \ref ->
          assertBool (path <> " contains stale slash-form tool reference: " <> T.unpack ref)
            (not ("stack_mcp/" `T.isPrefixOf` ref))

  , testCase "all MCP tool references map to real server tools" $ do
      manifests <- loadAgentManifests
      let validRefs = map (("stack_mcp_" <>) . toolName) allTools
      forM_ manifests $ \(path, refs) ->
        forM_ refs $ \ref ->
          if "stack_mcp_" `T.isPrefixOf` ref
            then assertBool (path <> " references unknown tool: " <> T.unpack ref)
                   (ref `elem` validRefs)
            else pure ()
  ]

loadAgentManifests :: IO [(FilePath, [Text])]
loadAgentManifests = do
  let agentDir = "agents"
  files <- listDirectory agentDir
  let manifestFiles = [agentDir </> f | f <- files, ".agent.md" `T.isSuffixOf` T.pack f]
  mapM loadManifest manifestFiles

loadManifest :: FilePath -> IO (FilePath, [Text])
loadManifest path = do
  content <- readFile path
  pure (path, extractToolRefs (T.lines (T.pack content)))

extractToolRefs :: [Text] -> [Text]
extractToolRefs ls =
  case dropWhile (/= "tools:") (frontMatter ls) of
    [] -> []
    (_ : rest) ->
      [ T.strip (T.drop 1 stripped)
      | line <- takeWhile isToolLine rest
      , let stripped = T.strip line
      ]

frontMatter :: [Text] -> [Text]
frontMatter ("---" : rest) = takeWhile (/= "---") rest
frontMatter _ = []

isToolLine :: Text -> Bool
isToolLine line = "- " `T.isPrefixOf` T.strip line
