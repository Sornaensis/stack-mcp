{-# LANGUAGE OverloadedStrings #-}

module StackMCP.Process
  ( runStackRaw
  , StackOutput(..)
  , defaultTimeoutUs
  ) where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import Data.Text qualified as T
import System.Process (proc, readCreateProcessWithExitCode, CreateProcess(..))
import System.Exit (ExitCode(..))
import System.Timeout (timeout)

-- | Default timeout for stack commands: 5 minutes (in microseconds).
defaultTimeoutUs :: Int
defaultTimeoutUs = 5 * 60 * 1000000

-- | Structured output from a stack command.
data StackOutput = StackOutput
  { soExitCode :: !Int
  , soStdout   :: !Text
  , soStderr   :: !Text
  } deriving (Show)

-- | Run a stack command and return raw structured output.
--   Passes --color=never to suppress ANSI escape codes.
--   Returns a synthetic exit code 127 if the process cannot be started.
--   Times out after 5 minutes with exit code 124.
runStackRaw :: Maybe FilePath -> [Text] -> IO StackOutput
runStackRaw mcwd args = do
  let strArgs = map T.unpack ("--color=never" : args)
      cp = (proc "stack" strArgs) { cwd = mcwd }
  mResult <- timeout defaultTimeoutUs $
    try (readCreateProcessWithExitCode cp "")
      :: IO (Maybe (Either SomeException (ExitCode, String, String)))
  case mResult of
    Nothing -> pure $ StackOutput 124 ""
      ("stack-mcp: command timed out after 5 minutes: stack " <> T.unwords args)
    Just (Left err) -> pure $ StackOutput 127 ""
      ("stack-mcp: failed to run stack: " <> T.pack (show err))
    Just (Right (exitCode, stdout', stderr')) ->
      let code = case exitCode of
            ExitSuccess   -> 0
            ExitFailure n -> n
      in pure $ StackOutput code (T.pack stdout') (T.pack stderr')
