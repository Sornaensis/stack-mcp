{-# LANGUAGE OverloadedStrings #-}

module CommonTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)

import StackMCP.Tools.Common

tests :: TestTree
tests = testGroup "Common"
  [ paramExtractionTests
  , schemaBuilderTests
  , tagEachTests
  ]

------------------------------------------------------------------------
-- Parameter Extraction
------------------------------------------------------------------------

paramExtractionTests :: TestTree
paramExtractionTests = testGroup "Parameter extraction"
  [ testCase "getParamText: present key" $
      getParamText "name" (object ["name" .= ("hello" :: Text)]) @?= "hello"

  , testCase "getParamText: missing key → empty" $
      getParamText "name" (object []) @?= ""

  , testCase "getParamText: non-string value → empty" $
      getParamText "name" (object ["name" .= (42 :: Int)]) @?= ""

  , testCase "getParamText: non-object input → empty" $
      getParamText "name" (String "nope") @?= ""

  , testCase "getParamBool: present true" $
      getParamBool "flag" (object ["flag" .= True]) @?= True

  , testCase "getParamBool: present false" $
      getParamBool "flag" (object ["flag" .= False]) @?= False

  , testCase "getParamBool: missing → False" $
      getParamBool "flag" (object []) @?= False

  , testCase "getParamBool: non-bool value → False" $
      getParamBool "flag" (object ["flag" .= ("yes" :: Text)]) @?= False

  , testCase "getParamInt: integer value" $
      getParamInt "count" (object ["count" .= (5 :: Int)]) @?= Just 5

  , testCase "getParamInt: missing → Nothing" $
      getParamInt "count" (object []) @?= Nothing

  , testCase "getParamInt: non-number → Nothing" $
      getParamInt "count" (object ["count" .= ("five" :: Text)]) @?= Nothing

  , testCase "getParamInt: fractional number → Nothing (exact match)" $
      getParamInt "count" (object ["count" .= (3.5 :: Double)]) @?= Nothing

  , testCase "getParamInt: whole number as double → Just" $
      getParamInt "count" (object ["count" .= (7.0 :: Double)]) @?= Just 7

  , testCase "getParamInt: negative integer" $
      getParamInt "count" (object ["count" .= (-3 :: Int)]) @?= Just (-3)

  , testCase "getParamInt: zero" $
      getParamInt "count" (object ["count" .= (0 :: Int)]) @?= Just 0

  , testCase "getParamArray: present" $
      getParamArray "items" (object ["items" .= (["a", "b", "c"] :: [Text])]) @?= ["a", "b", "c"]

  , testCase "getParamArray: missing → empty" $
      getParamArray "items" (object []) @?= []

  , testCase "getParamArray: non-array → empty" $
      getParamArray "items" (object ["items" .= ("nope" :: Text)]) @?= []

  , testCase "getParamArray: mixed array (non-strings dropped)" $
      getParamArray "items" (object ["items" .= [String "a", Number 1, String "b"]]) @?= ["a", "b"]

  , testCase "getParamArray: empty array" $
      getParamArray "items" (object ["items" .= ([] :: [Text])]) @?= []

  , testCase "getParamMaybeBool: present true" $
      getParamMaybeBool "flag" (object ["flag" .= True]) @?= Just True

  , testCase "getParamMaybeBool: present false" $
      getParamMaybeBool "flag" (object ["flag" .= False]) @?= Just False

  , testCase "getParamMaybeBool: missing -> Nothing" $
      getParamMaybeBool "flag" (object []) @?= Nothing

  , testCase "getParamMaybeBool: non-bool -> Nothing" $
      getParamMaybeBool "flag" (object ["flag" .= ("yes" :: Text)]) @?= Nothing
  ]

------------------------------------------------------------------------
-- Schema Builders
------------------------------------------------------------------------

schemaBuilderTests :: TestTree
schemaBuilderTests = testGroup "Schema builders"
  [ testCase "mkSchema basic" $ do
      let schema = mkSchema [("path", strProp "A file path")] ["path"]
      case schema of
        Object o -> do
          lk "type" o @?= Just (String "object")
          case lk "properties" o of
            Just (Object props) ->
              assertBool "has path property" (KM.member (Key.fromText "path") props)
            _ -> assertFailure "properties should be object"
          case lk "required" o of
            Just (Array arr) -> length arr @?= 1
            _ -> assertFailure "required should be array"
        _ -> assertFailure "schema should be object"

  , testCase "mkSchema no required" $ do
      let schema = mkSchema [("opt", boolProp "Optional")] []
      case schema of
        Object o -> do
          assertBool "no required key" (not $ KM.member (Key.fromText "required") o)
        _ -> assertFailure "schema should be object"

  , testCase "strProp" $ do
      let p = strProp "desc"
      case p of
        Object o -> lk "type" o @?= Just (String "string")
        _        -> assertFailure "strProp should be object"

  , testCase "boolProp" $ do
      let p = boolProp "desc"
      case p of
        Object o -> lk "type" o @?= Just (String "boolean")
        _        -> assertFailure "boolProp should be object"

  , testCase "intProp" $ do
      let p = intProp "desc"
      case p of
        Object o -> lk "type" o @?= Just (String "integer")
        _        -> assertFailure "intProp should be object"

  , testCase "enumProp" $ do
      let p = enumProp "desc" ["a", "b"]
      case p of
        Object o -> do
          lk "type" o @?= Just (String "string")
          case lk "enum" o of
            Just (Array arr) -> length arr @?= 2
            _ -> assertFailure "enum should be array"
        _ -> assertFailure "enumProp should be object"

  , testCase "arrayProp" $ do
      let p = arrayProp "desc"
      case p of
        Object o -> lk "type" o @?= Just (String "array")
        _        -> assertFailure "arrayProp should be object"
  ]

tagEachTests :: TestTree
tagEachTests = testGroup "tagEach"
  [ testCase "empty list → empty" $
      tagEach "--ta" [] @?= ([] :: [Text])

  , testCase "single element" $
      tagEach "--ta" ["foo"] @?= ["--ta", "foo"]

  , testCase "multiple elements get separate tags" $
      tagEach "--ta" ["-p", "mytest"] @?= ["--ta", "-p", "--ta", "mytest"]

  , testCase "works with --ba" $
      tagEach "--ba" ["--match", "bench1", "--csv"] @?= ["--ba", "--match", "--ba", "bench1", "--ba", "--csv"]
  ]

lk :: Text -> Object -> Maybe Value
lk k o = KM.lookup (Key.fromText k) o
