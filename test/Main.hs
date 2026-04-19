module Main (main) where

import Test.Tasty
import System.IO (hSetEncoding, stdout, stderr, utf8)

import qualified AgentManifestTests
import qualified ParseTests
import qualified CommonTests
import qualified TypesTests
import qualified ToolDefsTests
import qualified PropertyTests
import qualified ServerTests

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  defaultMain $ testGroup "stack-mcp"
    [ AgentManifestTests.tests
    , ParseTests.tests
    , CommonTests.tests
    , TypesTests.tests
    , ToolDefsTests.tests
    , PropertyTests.tests
    , ServerTests.tests
    ]
