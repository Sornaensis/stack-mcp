{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PropertyTests (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Foldable (toList)
import Data.List (nub)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V

import StackMCP.Types
import StackMCP.Tools (allTools)
import StackMCP.Tools.Common
import StackMCP.Tools.Parse

tests :: TestTree
tests = testGroup "Properties"
  [ toolDefProperties
  , schemaProperties
  , paramExtractionProperties
  , tagEachProperties
  , toolResultProperties
  , parseProperties
  ]

------------------------------------------------------------------------
-- Generators
------------------------------------------------------------------------

-- | Arbitrary non-empty Text
newtype NonEmptyText = NonEmptyText { unNET :: Text }
  deriving (Show, Eq)

instance Arbitrary NonEmptyText where
  arbitrary = NonEmptyText . T.pack <$> listOf1 arbitraryPrintableChar
  shrink (NonEmptyText t) =
    [ NonEmptyText (T.pack s) | s <- shrink (T.unpack t), not (null s) ]

-- | Arbitrary Text (may be empty)
newtype ArbitraryText = ArbitraryText { unAT :: Text }
  deriving (Show, Eq)

instance Arbitrary ArbitraryText where
  arbitrary = ArbitraryText . T.pack <$> listOf arbitraryPrintableChar
  shrink (ArbitraryText t) =
    [ ArbitraryText (T.pack s) | s <- shrink (T.unpack t) ]

-- | Generate an Object Value with specific key-value pairs
mkObj :: [(Text, Value)] -> Value
mkObj kvs = object [ Key.fromText k .= v | (k, v) <- kvs ]

------------------------------------------------------------------------
-- ToolDef properties
------------------------------------------------------------------------

toolDefProperties :: TestTree
toolDefProperties = testGroup "ToolDef"
  [ testProperty "all tool names are non-empty" $
      all (not . T.null . toolName) allTools

  , testProperty "all tool descriptions are non-empty" $
      all (not . T.null . toolDescription) allTools

  , testProperty "all tool names are unique" $
      let names = map toolName allTools
      in names === nub names

  , testProperty "every ToolDef serializes to object with name+description+inputSchema" $
      conjoin [ case toJSON td of
                  Object o ->
                    counterexample (T.unpack (toolName td)) $
                      KM.member "name" o .&&.
                      KM.member "description" o .&&.
                      KM.member "inputSchema" o
                  _ -> counterexample (T.unpack (toolName td) ++ " not an object") False
              | td <- allTools
              ]

  , testProperty "serialized name matches toolName" $
      conjoin [ case toJSON td of
                  Object o -> case KM.lookup "name" o of
                    Just (String n) -> n === toolName td
                    _               -> counterexample "name not a string" False
                  _ -> counterexample "not an object" False
              | td <- allTools
              ]
  ]

------------------------------------------------------------------------
-- Schema properties
------------------------------------------------------------------------

schemaProperties :: TestTree
schemaProperties = testGroup "Schema"
  [ testProperty "mkSchema always produces type=object" $ \(props :: [(NonEmptyText, NonEmptyText)]) ->
      let ps = [(unNET k, strProp (unNET v)) | (k, v) <- props]
          schema = mkSchema ps []
      in case schema of
           Object o -> KM.lookup "type" o === Just (String "object")
           _ -> counterexample "not an object" False

  , testProperty "mkSchema required only present when non-empty" $ \(reqs :: [NonEmptyText]) ->
      let schema = mkSchema [] (map unNET reqs)
      in case schema of
           Object o -> case reqs of
             [] -> KM.member "required" o === False
             _  -> KM.member "required" o === True
           _ -> counterexample "not an object" False

  , testProperty "mkSchema properties count matches input" $ \(props :: [(NonEmptyText, NonEmptyText)]) ->
      let uniqueProps = nub [(unNET k, strProp (unNET v)) | (k, v) <- props]
          schema = mkSchema uniqueProps []
      in case schema of
           Object o -> case KM.lookup "properties" o of
             Just (Object ps) -> KM.size ps === length uniqueProps
             _ -> counterexample "properties missing" False
           _ -> counterexample "not an object" False

  , testProperty "mkSchema required fields are subset of properties" $
      conjoin [ let schema = toolInputSchema td
                in case schema of
                     Object o -> case (KM.lookup "properties" o, KM.lookup "required" o) of
                       (Just (Object ps), Just (Array reqs)) ->
                         counterexample (T.unpack (toolName td)) $
                           conjoin [ case r of
                                       String rn -> counterexample (T.unpack rn ++ " in properties") $
                                         KM.member (Key.fromText rn) ps === True
                                       _ -> counterexample "required entry not a string" False
                                   | r <- toList reqs
                                   ]
                       (_, Nothing) -> property True  -- no required = fine
                       _ -> counterexample (T.unpack (toolName td) ++ ": bad schema shape") False
                     _ -> counterexample (T.unpack (toolName td) ++ ": schema not object") False
              | td <- allTools
              ]

  , testProperty "all property values are objects with type field" $
      conjoin [ case toolInputSchema td of
                  Object o -> case KM.lookup "properties" o of
                    Just (Object ps) ->
                      counterexample (T.unpack (toolName td)) $
                        conjoin [ case v of
                                    Object pv -> counterexample (show k ++ " has type") $
                                      KM.member "type" pv === True
                                    _ -> counterexample (show k ++ " not object") False
                                | (k, v) <- KM.toList ps
                                ]
                    _ -> counterexample (T.unpack (toolName td) ++ " no properties") False
                  _ -> property False
              | td <- allTools
              ]

  , testProperty "all property descriptions are non-empty" $
      conjoin [ case toolInputSchema td of
                  Object o -> case KM.lookup "properties" o of
                    Just (Object ps) ->
                      conjoin [ case v of
                                  Object pv -> case KM.lookup "description" pv of
                                    Just (String d) -> counterexample (T.unpack (toolName td) ++ "." ++ show k) $
                                      T.null d === False
                                    _ -> counterexample (T.unpack (toolName td) ++ "." ++ show k ++ " no desc") False
                                  _ -> property True
                              | (k, v) <- KM.toList ps
                              ]
                    _ -> property True
                  _ -> property True
              | td <- allTools
              ]
  ]

------------------------------------------------------------------------
-- Parameter extraction properties
------------------------------------------------------------------------

paramExtractionProperties :: TestTree
paramExtractionProperties = testGroup "Parameter extraction"
  [ testProperty "getParamText returns empty on non-object" $ \(v :: Value) ->
      case v of
        Object _ -> property True  -- skip objects
        _        -> getParamText "anything" v === ""

  , testProperty "getParamText roundtrips string values" $ \(ArbitraryText t) ->
      getParamText "k" (mkObj [("k", String t)]) === t

  , testProperty "getParamText returns empty for non-string" $ \(v :: Value) ->
      case v of
        String _ -> property True  -- skip strings
        _        -> getParamText "k" (mkObj [("k", v)]) === ""

  , testProperty "getParamBool returns False on non-object" $ \(v :: Value) ->
      case v of
        Object _ -> property True
        _        -> getParamBool "anything" v === False

  , testProperty "getParamBool roundtrips" $ \(b :: Bool) ->
      getParamBool "k" (mkObj [("k", Bool b)]) === b

  , testProperty "getParamMaybeBool Nothing on missing key" $ \(ArbitraryText k) ->
      getParamMaybeBool k (mkObj []) === Nothing

  , testProperty "getParamMaybeBool Just on present bool" $ \(b :: Bool) ->
      getParamMaybeBool "k" (mkObj [("k", Bool b)]) === Just b

  , testProperty "getParamInt roundtrips integers" $ \(n :: Int) ->
      getParamInt "k" (mkObj [("k", Number (fromIntegral n))]) === Just n

  , testProperty "getParamInt Nothing on non-object" $ \(v :: Value) ->
      case v of
        Object _ -> property True
        _        -> getParamInt "anything" v === Nothing

  , testProperty "getParamArray returns strings only" $ \(ts :: [ArbitraryText]) ->
      let arr = Array (V.fromList (map (String . unAT) ts))
      in getParamArray "k" (mkObj [("k", arr)]) === map unAT ts

  , testProperty "getParamArray empty on non-object" $ \(v :: Value) ->
      case v of
        Object _ -> property True
        _        -> getParamArray "anything" v === []

  , testProperty "getParamArray filters non-strings" $ \(vs :: [Value]) ->
      let arr = Array (V.fromList vs)
          expected = [ t | String t <- vs ]
      in getParamArray "k" (mkObj [("k", arr)]) === expected

  , testProperty "getParamText missing key is empty" $ \(ArbitraryText k) ->
      not (T.null k) ==>
        getParamText k (mkObj []) === ""

  , testProperty "getParamBool missing key is False" $ \(ArbitraryText k) ->
      not (T.null k) ==>
        getParamBool k (mkObj []) === False

  , testProperty "getParamInt missing key is Nothing" $ \(ArbitraryText k) ->
      not (T.null k) ==>
        getParamInt k (mkObj []) === Nothing
  ]

------------------------------------------------------------------------
-- tagEach properties
------------------------------------------------------------------------

tagEachProperties :: TestTree
tagEachProperties = testGroup "tagEach"
  [ testProperty "length is doubled" $ \(ts :: [ArbitraryText]) ->
      let items = map unAT ts
      in length (tagEach "--ta" items) === 2 * length items

  , testProperty "empty input → empty output" $ \(ArbitraryText tag) ->
      tagEach tag [] === ([] :: [Text])

  , testProperty "every odd element is the tag" $ \(ts :: [ArbitraryText]) ->
      let items = map unAT ts
          result = tagEach "--flag" items
          odds = [ result !! i | i <- [0, 2 .. length result - 1] ]
      in not (null items) ==>
           all (== "--flag") odds

  , testProperty "every even element is from original list" $ \(ts :: [ArbitraryText]) ->
      let items = map unAT ts
          result = tagEach "--flag" items
          evens = [ result !! i | i <- [1, 3 .. length result - 1] ]
      in not (null items) ==>
           evens === items

  , testProperty "preserves order" $ \(ts :: [ArbitraryText]) ->
      let items = map unAT ts
          result = tagEach "--x" items
          extracted = [ result !! i | i <- [1, 3 .. length result - 1] ]
      in extracted === items
  ]

------------------------------------------------------------------------
-- ToolResult properties
------------------------------------------------------------------------

toolResultProperties :: TestTree
toolResultProperties = testGroup "ToolResult"
  [ testProperty "mkToolResult is not error" $ \(ArbitraryText t) ->
      toolResultIsError (mkToolResult t) === False

  , testProperty "mkToolError is error" $ \(ArbitraryText t) ->
      toolResultIsError (mkToolError t) === True

  , testProperty "mkToolResult content is non-empty" $ \(ArbitraryText t) ->
      null (toolResultContent (mkToolResult t)) === False

  , testProperty "mkToolError content is non-empty" $ \(ArbitraryText t) ->
      null (toolResultContent (mkToolError t)) === False

  , testProperty "mkToolResult serializes to object with content+isError" $ \(ArbitraryText t) ->
      case toJSON (mkToolResult t) of
        Object o -> KM.member "content" o .&&. KM.member "isError" o
        _ -> counterexample "not an object" False

  , testProperty "mkToolResultJSON is not error" $ \(v :: Value) ->
      toolResultIsError (mkToolResultJSON v) === False

  , testProperty "mkToolErrorJSON is error" $ \(v :: Value) ->
      toolResultIsError (mkToolErrorJSON v) === True

  , testProperty "mkStructuredError is error with fields" $ \(ArbitraryText msg) ->
      let r = mkStructuredError "test_error" msg ["cmd"] []
      in toolResultIsError r === True

  , testProperty "mkCommandError always produces error" $ \(ts :: [ArbitraryText]) ->
      let args = map unAT ts
          so = StackOutput 1 "out" "err"
      in toolResultIsError (mkCommandError args so) === True

  , testProperty "mkCommandError with exit 0 still error" $ \(ts :: [ArbitraryText]) ->
      let args = map unAT ts
          so = StackOutput 0 "" ""
      in toolResultIsError (mkCommandError args so) === True
  ]

------------------------------------------------------------------------
-- Parse properties
------------------------------------------------------------------------

parseProperties :: TestTree
parseProperties = testGroup "Parsing"
  [ testProperty "parseGhcDiagnostics never crashes on arbitrary text" $ \(ArbitraryText t) ->
      let diags = parseGhcDiagnostics t
      in length diags `seq` True

  , testProperty "parseTestFailures never crashes on arbitrary text" $ \(ArbitraryText t) ->
      let fails = parseTestFailures t
      in length fails `seq` True

  , testProperty "parseGhcDiagnostics empty on empty input" $
      null (parseGhcDiagnostics "") === True

  , testProperty "parseTestFailures empty on empty input" $
      null (parseTestFailures "") === True

  , testProperty "diagnosticsSummary always has required fields" $ \(ArbitraryText t) ->
      let diags = parseGhcDiagnostics t
          summary = diagnosticsSummary diags
      in case summary of
           Object o -> KM.member "error_count" o .&&. KM.member "warning_count" o .&&.
                       KM.member "errors" o .&&. KM.member "warnings" o
           _ -> counterexample "not an object" False

  , testProperty "diagnosticsSummary counts are non-negative" $ \(ArbitraryText t) ->
      let diags = parseGhcDiagnostics t
          summary = diagnosticsSummary diags
      in case summary of
           Object o -> conjoin
             [ case KM.lookup (Key.fromText k) o of
                 Just (Number n) -> counterexample (T.unpack k) $ n >= 0
                 _ -> counterexample (T.unpack k ++ " missing or not number") False
             | k <- ["error_count", "warning_count"]
             ]
           _ -> counterexample "not an object" False

  , testProperty "diagnosticsSummary error_count + warning_count = list lengths" $ \(ArbitraryText t) ->
      let diags = parseGhcDiagnostics t
          summary = diagnosticsSummary diags
      in case summary of
           Object o ->
             let getN k = case KM.lookup (Key.fromText k) o of
                            Just (Number n) -> n
                            _               -> -1
                 getLen k = case KM.lookup (Key.fromText k) o of
                              Just (Array arr) -> fromIntegral (length arr)
                              _                -> -2
                 ec = getN "error_count"
                 wc = getN "warning_count"
                 el = getLen "errors"
                 wl = getLen "warnings"
             in counterexample (show (ec, wc, el, wl)) $
                  (ec === el) .&&. (wc === wl)
           _ -> counterexample "not an object" False

  , testProperty "testFailuresSummary always has required fields" $ \(ArbitraryText t) ->
      let fails = parseTestFailures t
          summary = testFailuresSummary fails
      in case summary of
           Object o -> KM.member "failure_count" o .&&. KM.member "failures" o
           _ -> counterexample "not an object" False

  , testProperty "testFailuresSummary count matches list" $ \(ArbitraryText t) ->
      let fails = parseTestFailures t
          summary = testFailuresSummary fails
      in case summary of
           Object o -> case (KM.lookup "failure_count" o, KM.lookup "failures" o) of
             (Just (Number n), Just (Array arr)) ->
               n === fromIntegral (length arr)
             _ -> counterexample "unexpected shape" False
           _ -> counterexample "not an object" False

  , testProperty "parsed diagnostics have valid severity" $ \(ArbitraryText t) ->
      let diags = parseGhcDiagnostics t
      in conjoin [ diagSeverity d `elem` [SevError, SevWarning] | d <- diags ]

  , testProperty "parsed diagnostics have non-empty file" $ \(ArbitraryText t) ->
      let diags = parseGhcDiagnostics t
      in conjoin [ counterexample "empty file" $ T.null (diagFile d) === False | d <- diags ]

  , testProperty "parsed diagnostics have positive line numbers" $ \(ArbitraryText t) ->
      let diags = parseGhcDiagnostics t
      in conjoin [ counterexample "line <= 0" $ diagLine d > 0 | d <- diags ]

  , testProperty "parsed diagnostics have positive column numbers" $ \(ArbitraryText t) ->
      let diags = parseGhcDiagnostics t
      in conjoin [ counterexample "col <= 0" $ diagColumn d > 0 | d <- diags ]
  ]
