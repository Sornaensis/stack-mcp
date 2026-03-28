{-# LANGUAGE OverloadedStrings #-}

module TypesTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Maybe (isNothing)

import StackMCP.Types

tests :: TestTree
tests = testGroup "Types"
  [ contentBlockTests
  , toolResultTests
  , jsonRpcTests
  , resourceTests
  , promptTests
  ]

------------------------------------------------------------------------
-- ContentBlock
------------------------------------------------------------------------

contentBlockTests :: TestTree
contentBlockTests = testGroup "ContentBlock"
  [ testCase "textContent" $ do
      let cb = textContent "hello"
      contentType cb @?= "text"
      contentText cb @?= "hello"

  , testCase "jsonContent roundtrip" $ do
      let val = object ["key" .= ("value" :: Text)]
          cb  = jsonContent val
      contentType cb @?= "text"
      -- The text should be valid JSON that decodes back
      case decodeStrict (TE.encodeUtf8 (contentText cb)) of
        Just (Object o) -> lk "key" o @?= Just (String "value")
        _               -> assertFailure "jsonContent text should be valid JSON"
  ]

------------------------------------------------------------------------
-- ToolResult construction
------------------------------------------------------------------------

toolResultTests :: TestTree
toolResultTests = testGroup "ToolResult"
  [ testCase "mkToolResult is not error" $ do
      let tr = mkToolResult "success"
      toolResultIsError tr @?= False
      length (toolResultContent tr) @?= 1
      contentText (head (toolResultContent tr)) @?= "success"

  , testCase "mkToolError is error" $ do
      let tr = mkToolError "failure"
      toolResultIsError tr @?= True
      contentText (head (toolResultContent tr)) @?= "failure"

  , testCase "mkToolResultJSON" $ do
      let val = object ["status" .= ("ok" :: Text)]
          tr = mkToolResultJSON val
      toolResultIsError tr @?= False
      -- Content should be JSON text
      case decodeStrict (TE.encodeUtf8 (contentText (head (toolResultContent tr)))) of
        Just (Object o) -> lk "status" o @?= Just (String "ok")
        _               -> assertFailure "mkToolResultJSON content should be valid JSON"

  , testCase "mkToolErrorJSON" $ do
      let val = object ["error" .= ("bad" :: Text)]
          tr = mkToolErrorJSON val
      toolResultIsError tr @?= True

  , testCase "mkStructuredError" $ do
      let tr = mkStructuredError "test_err" "msg" ["stack", "build"] ["try again"]
      toolResultIsError tr @?= True
      case decodeStrict (TE.encodeUtf8 (contentText (head (toolResultContent tr)))) of
        Just (Object o) -> do
          lk "error_type" o @?= Just (String "test_err")
          lk "message" o @?= Just (String "msg")
        _ -> assertFailure "mkStructuredError should produce valid JSON"

  , testCase "mkToolResult ToJSON" $ do
      let tr = mkToolResult "hello"
          val = toJSON tr
      case val of
        Object o -> do
          lk "isError" o @?= Just (Bool False)
          case lk "content" o of
            Just (Array arr) -> length arr @?= 1
            _ -> assertFailure "content should be array"
        _ -> assertFailure "ToolResult should serialize to object"
  ]

------------------------------------------------------------------------
-- JsonRpc helpers
------------------------------------------------------------------------

jsonRpcTests :: TestTree
jsonRpcTests = testGroup "JsonRpc"
  [ testCase "mkSuccess" $ do
      let resp = mkSuccess (Just (Number 1)) (String "ok")
          val  = toJSON resp
      case val of
        Object o -> do
          lk "jsonrpc" o @?= Just (String "2.0")
          lk "id" o @?= Just (Number 1)
          lk "result" o @?= Just (String "ok")
          assertBool "no error field" (isNothing (lk "error" o))
        _ -> assertFailure "response should be object"

  , testCase "mkError" $ do
      let resp = mkError (Just (Number 2)) (-32601) "Method not found"
          val  = toJSON resp
      case val of
        Object o -> do
          lk "jsonrpc" o @?= Just (String "2.0")
          lk "id" o @?= Just (Number 2)
          assertBool "no result" (isNothing (lk "result" o))
          case lk "error" o of
            Just (Object e) -> do
              lk "code" e @?= Just (Number (-32601))
              lk "message" e @?= Just (String "Method not found")
            _ -> assertFailure "error should be object"
        _ -> assertFailure "response should be object"

  , testCase "mkSuccess with null id" $ do
      let resp = mkSuccess Nothing (String "ok")
          val  = toJSON resp
      case val of
        Object o -> lk "id" o @?= Just Null
        _        -> assertFailure "response should be object"

  , testCase "JsonRpcRequest FromJSON" $ do
      let raw = object
            [ "jsonrpc" .= ("2.0" :: Text)
            , "id"      .= (1 :: Int)
            , "method"  .= ("test" :: Text)
            , "params"  .= object ["key" .= ("val" :: Text)]
            ]
      case fromJSON raw :: Result JsonRpcRequest of
        Success req -> do
          rpcReqJsonrpc req @?= "2.0"
          rpcReqMethod req @?= "test"
        Error e -> assertFailure ("parse failed: " ++ e)
  ]

------------------------------------------------------------------------
-- Resources
------------------------------------------------------------------------

resourceTests :: TestTree
resourceTests = testGroup "Resources"
  [ testCase "ResourceDef ToJSON" $ do
      let rd = ResourceDef "stack://project/stack.yaml" "stack.yaml" (Just "Stack config") (Just "text/yaml")
          val = toJSON rd
      case val of
        Object o -> do
          lk "uri" o @?= Just (String "stack://project/stack.yaml")
          lk "name" o @?= Just (String "stack.yaml")
        _ -> assertFailure "ResourceDef should be object"

  , testCase "ResourceDef minimal ToJSON" $ do
      let rd = ResourceDef "stack://project/f" "f" Nothing Nothing
          val = toJSON rd
      case val of
        Object o -> do
          assertBool "no description" (isNothing (lk "description" o))
          assertBool "no mimeType" (isNothing (lk "mimeType" o))
        _ -> assertFailure "ResourceDef should be object"

  , testCase "ResourceContent ToJSON" $ do
      let rc = ResourceContent "stack://project/stack.yaml" (Just "text/yaml") "resolver: lts-24.2"
          val = toJSON rc
      case val of
        Object o -> do
          lk "uri" o @?= Just (String "stack://project/stack.yaml")
          lk "text" o @?= Just (String "resolver: lts-24.2")
        _ -> assertFailure "ResourceContent should be object"
  ]

------------------------------------------------------------------------
-- Prompts
------------------------------------------------------------------------

promptTests :: TestTree
promptTests = testGroup "Prompts"
  [ testCase "PromptDef ToJSON" $ do
      let pd = PromptDef "test-prompt" (Just "A test") [PromptArgument "arg1" (Just "First arg") True]
          val = toJSON pd
      case val of
        Object o -> do
          lk "name" o @?= Just (String "test-prompt")
        _ -> assertFailure "PromptDef should be object"

  , testCase "PromptMessage ToJSON" $ do
      let pm = PromptMessage "user" (textContent "hello")
          val = toJSON pm
      case val of
        Object o -> do
          lk "role" o @?= Just (String "user")
        _ -> assertFailure "PromptMessage should be object"
  ]

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

lk :: Text -> Object -> Maybe Value
lk k o = KM.lookup (Key.fromText k) o
