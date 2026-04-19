{-# LANGUAGE OverloadedStrings #-}

module ServerTests (tests) where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.List (sort)
import Data.Text (Text)
import Data.Vector qualified as V
import Test.Tasty
import Test.Tasty.HUnit

import StackMCP.Server (handleToolsList)
import StackMCP.Tools (allTools)
import StackMCP.Types (JsonRpcRequest(..), JsonRpcResponse(..), ToolDef(..))

tests :: TestTree
tests = testGroup "Server"
  [ testCase "tools/list returns every tool in the first response" $ do
      let req = JsonRpcRequest
            { rpcReqJsonrpc = "2.0"
            , rpcReqId = Just (Number 1)
            , rpcReqMethod = "tools/list"
            , rpcReqParams = Nothing
            }
      resp <- handleToolsList req
      case rpcResResult resp of
        Just (Object result) -> do
          assertBool "tools/list should not paginate the initial response"
            (not (KM.member (Key.fromText "nextCursor") result))
          case KM.lookup (Key.fromText "tools") result of
            Just (Array tools) -> do
              let actualNames = sort $ V.toList $ V.mapMaybe toolNameFromValue tools
                  expectedNames = sort (map toolName allTools)
              actualNames @?= expectedNames
            _ -> assertFailure "tools/list result should contain a tools array"
        _ -> assertFailure "tools/list should return a result object"
  ]

toolNameFromValue :: Value -> Maybe Text
toolNameFromValue (Object o) = case KM.lookup (Key.fromText "name") o of
  Just (String name) -> Just name
  _                  -> Nothing
toolNameFromValue _ = Nothing
