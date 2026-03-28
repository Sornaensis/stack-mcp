{-# LANGUAGE OverloadedStrings #-}

module ParseTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Data.Text qualified as T

import StackMCP.Tools.Parse

tests :: TestTree
tests = testGroup "Parse"
  [ ghcDiagnosticTests
  , testFailureTests
  ]

------------------------------------------------------------------------
-- GHC Diagnostic Parsing
------------------------------------------------------------------------

ghcDiagnosticTests :: TestTree
ghcDiagnosticTests = testGroup "GHC Diagnostics"
  [ testCase "single error" $ do
      let input = "src/Foo.hs:10:1: error: [GHC-88464]\n    Variable not in scope: foo"
          diags = parseGhcDiagnostics input
      length diags @?= 1
      let d = head diags
      diagFile d @?= "src/Foo.hs"
      diagLine d @?= 10
      diagColumn d @?= 1
      diagSeverity d @?= SevError
      diagCode d @?= Just "[GHC-88464]"
      assertBool "message contains variable" (T.isInfixOf "Variable not in scope" (diagMessage d))

  , testCase "single warning" $ do
      let input = "src/Bar.hs:5:3: warning: [-Wunused-imports]\n    The import of 'Data.List' is redundant"
          diags = parseGhcDiagnostics input
      length diags @?= 1
      let d = head diags
      diagFile d @?= "src/Bar.hs"
      diagLine d @?= 5
      diagColumn d @?= 3
      diagSeverity d @?= SevWarning
      diagCode d @?= Just "[-Wunused-imports]"

  , testCase "multiple diagnostics" $ do
      let input = T.unlines
            [ "src/A.hs:1:1: error: Module not found"
            , "src/B.hs:20:5: warning: Unused variable 'x'"
            , "src/C.hs:30:1: error: [GHC-12345] Type mismatch"
            ]
          diags = parseGhcDiagnostics input
      length diags @?= 3
      diagSeverity (diags !! 0) @?= SevError
      diagSeverity (diags !! 1) @?= SevWarning
      diagSeverity (diags !! 2) @?= SevError

  , testCase "Windows path with drive letter" $ do
      let input = "C:\\Users\\dev\\src\\Foo.hs:10:1: error: Parse error"
          diags = parseGhcDiagnostics input
      length diags @?= 1
      diagFile (head diags) @?= "C:\\Users\\dev\\src\\Foo.hs"
      diagLine (head diags) @?= 10

  , testCase "multiline error body" $ do
      let input = T.unlines
            [ "src/Foo.hs:10:1: error:"
            , "    Could not find module 'Data.Aeson'"
            , "    It is not a member of the package"
            , "    Use -v to see a list"
            ]
          diags = parseGhcDiagnostics input
      length diags @?= 1
      assertBool "message has multiple lines" (T.isInfixOf "Data.Aeson" (diagMessage (head diags)))

  , testCase "ignores stack noise" $ do
      let input = T.unlines
            [ "Preprocessing library for pkg-0.1.0.0..."
            , "Building library for pkg-0.1.0.0..."
            , "[ 1 of 3] Compiling Foo"
            , "src/Foo.hs:10:1: error: Type error"
            , "[ 2 of 3] Compiling Bar"
            ]
          diags = parseGhcDiagnostics input
      length diags @?= 1
      diagFile (head diags) @?= "src/Foo.hs"

  , testCase "empty input" $ do
      let diags = parseGhcDiagnostics ""
      length diags @?= 0

  , testCase "no diagnostics in noise-only input" $ do
      let input = T.unlines
            [ "Preprocessing library..."
            , "Building library..."
            , "[ 1 of 3] Compiling Foo"
            ]
          diags = parseGhcDiagnostics input
      length diags @?= 0

  , testCase "diagnosticsSummary counts" $ do
      let input = T.unlines
            [ "src/A.hs:1:1: error: err1"
            , "src/B.hs:2:1: warning: warn1"
            , "src/C.hs:3:1: error: err2"
            ]
          diags = parseGhcDiagnostics input
          summary = diagnosticsSummary diags
      case summary of
        Object o -> do
          lookupKey "error_count" o @?= Just (Number 2)
          lookupKey "warning_count" o @?= Just (Number 1)
        _ -> assertFailure "summary should be an Object"

  , testCase "error without code" $ do
      let input = "src/Foo.hs:10:1: error: Something went wrong"
          diags = parseGhcDiagnostics input
      length diags @?= 1
      diagCode (head diags) @?= Nothing
      assertBool "message present" (T.isInfixOf "Something went wrong" (diagMessage (head diags)))

  , testCase "empty message preserves location info" $ do
      let input = "src/Foo.hs:10:1: error:"
          diags = parseGhcDiagnostics input
      length diags @?= 1
      let d = head diags
      diagFile d @?= "src/Foo.hs"
      diagLine d @?= 10
      diagColumn d @?= 1
      -- message should contain file location rather than being empty
      assertBool "message not empty" (not (T.null (diagMessage d)))
  ]

------------------------------------------------------------------------
-- Test Failure Parsing
------------------------------------------------------------------------

testFailureTests :: TestTree
testFailureTests = testGroup "Test Failures"
  [ testCase "hspec failures" $ do
      let input = T.unlines
            [ "Failures:"
            , ""
            , "  src/Test.hs:15:3:"
            , "  1) Module.function should do thing"
            , "       expected: True"
            , "        but got: False"
            , ""
            , "  src/Test.hs:20:3:"
            , "  2) Module.other should work"
            , "       expected: 42"
            , "        but got: 0"
            ]
          failures = parseTestFailures input
      length failures @?= 2
      tfName (failures !! 0) @?= "Module.function should do thing"
      assertBool "details contain expected" (T.isInfixOf "expected: True" (tfDetails (head failures)))
      tfLocation (failures !! 0) @?= Just "src/Test.hs:15:3"

  , testCase "hspec with 4-space indent details" $ do
      let input = T.unlines
            [ "  1) should work"
            , "    expected: 1"
            , "     but got: 2"
            ]
          failures = parseTestFailures input
      length failures @?= 1
      assertBool "details captured" (T.isInfixOf "expected: 1" (tfDetails (head failures)))

  , testCase "tasty FAIL" $ do
      let input = T.unlines
            [ "  Tests"
            , "    unit tests"
            , "      addition:       OK"
            , "      subtraction:    FAIL"
            , "        expected: 5"
            , "         but got: 3"
            , "      multiply:       OK"
            ]
          failures = parseTestFailures input
      length failures @?= 1
      tfName (head failures) @?= "subtraction"
      assertBool "details present" (T.isInfixOf "expected: 5" (tfDetails (head failures)))

  , testCase "tasty does not false-positive on word FAIL" $ do
      -- A line like "FAILURE handling: OK" should not trigger
      let input = T.unlines
            [ "  Tests"
            , "    FAILURE handling:  OK"
            , "    normal test:      OK"
            ]
          failures = parseTestFailures input
      length failures @?= 0

  , testCase "empty input" $ do
      let failures = parseTestFailures ""
      length failures @?= 0

  , testCase "testFailuresSummary" $ do
      let input = T.unlines
            [ "  1) test one"
            , "       expected: A"
            , "        but got: B"
            , "  2) test two"
            , "       expected: C"
            , "        but got: D"
            ]
          failures = parseTestFailures input
          summary = testFailuresSummary failures
      case summary of
        Object o -> do
          lookupKey "failure_count" o @?= Just (Number 2)
        _ -> assertFailure "summary should be an Object"

  , testCase "tab-indented details" $ do
      let input = T.unlines
            [ "  1) test with tabs"
            , "\t\texpected: foo"
            , "\t\t but got: bar"
            ]
          failures = parseTestFailures input
      length failures @?= 1
      assertBool "details captured" (T.isInfixOf "expected: foo" (tfDetails (head failures)))

  , testCase "tasty tab-indented details collected" $ do
      let input = T.unlines
            [ "  Tests"
            , "    mytest:  FAIL"
            , "\t\texpected: 10"
            , "\t\t but got: 20"
            , "    other:   OK"
            ]
          failures = parseTestFailures input
      length failures @?= 1
      tfName (head failures) @?= "mytest"
      assertBool "tab details present" (T.isInfixOf "expected: 10" (tfDetails (head failures)))

  , testCase "tasty FAIL with exact colon suffix" $ do
      -- "FAIL" alone without colon prefix should not match
      let input = T.unlines
            [ "  Some random FAIL text"
            , "  next line"
            ]
          failures = parseTestFailures input
      length failures @?= 0
  ]

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

lookupKey :: Text -> Object -> Maybe Value
lookupKey k o = KM.lookup (Key.fromText k) o

