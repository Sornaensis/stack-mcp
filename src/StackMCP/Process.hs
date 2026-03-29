{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StackMCP.Process
  ( runStackRaw
  , StackOutput(..)
  , defaultTimeoutUs
  ) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, try, catch)
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.IO (Handle, hClose)
import System.Process
  (proc, createProcess, waitForProcess, terminateProcess,
   CreateProcess(..), StdStream(..))
import System.Exit (ExitCode(..))
import System.Timeout (timeout)

-- | Default timeout for stack commands: 5 minutes (in microseconds).
defaultTimeoutUs :: Int
defaultTimeoutUs = 5 * 60 * 1000000

-- | Grace period after process exits for pipe readers to finish (10 seconds).
pipeGraceUs :: Int
pipeGraceUs = 10 * 1000000

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
--
--   On Windows, child processes spawned by @stack@ can inherit pipe handles.
--   Even after @stack@ exits, those children may hold the handles open,
--   causing @hGetContents@ to block indefinitely.  To avoid this we:
--     1. Wait for the process in a separate thread (interruptible timeout).
--     2. After the process exits (or on timeout), close the pipe handles
--        from our side so the reader threads get an error/EOF.
--     3. Give readers a bounded grace period before returning.
runStackRaw :: Maybe FilePath -> [Text] -> IO StackOutput
runStackRaw mcwd args = do
  let strArgs = map T.unpack ("--color=never" : args)
      cp = (proc "stack" strArgs)
             { cwd     = mcwd
             , std_in  = CreatePipe
             , std_out = CreatePipe
             , std_err = CreatePipe
             }
  result <- try $ do
    (Just hin, Just hout, Just herr, ph) <- createProcess cp
    hClose hin
    -- Accumulate output incrementally so closing the handle doesn't lose data.
    outRef <- newIORef T.empty
    errRef <- newIORef T.empty
    outVar <- newEmptyMVar
    errVar <- newEmptyMVar
    _ <- forkIO (readHandle hout outRef >>= putMVar outVar)
    _ <- forkIO (readHandle herr errRef >>= putMVar errVar)
    -- Wait for the process in a separate thread so the timeout is always
    -- interruptible.
    exitVar <- newEmptyMVar
    _ <- forkIO (waitForProcess ph >>= putMVar exitVar)
    mExit <- timeout defaultTimeoutUs (takeMVar exitVar)
    case mExit of
      Nothing -> do
        terminateProcess ph
        closeSilent hout
        closeSilent herr
        _ <- timeout pipeGraceUs (takeMVar outVar)
        _ <- timeout pipeGraceUs (takeMVar errVar)
        outStr <- readIORef outRef
        errStr <- readIORef errRef
        pure $ StackOutput 124 outStr
          ("stack-mcp: command timed out after 5 minutes: stack "
           <> T.unwords args <> if T.null errStr then "" else "\n" <> errStr)
      Just ec -> do
        -- Process exited, but child processes may still hold pipes open.
        -- Close handles from our side to unblock the reader threads.
        closeSilent hout
        closeSilent herr
        _ <- timeout pipeGraceUs (takeMVar outVar)
        _ <- timeout pipeGraceUs (takeMVar errVar)
        outStr <- readIORef outRef
        errStr <- readIORef errRef
        let code = case ec of
              ExitSuccess   -> 0
              ExitFailure n -> n
        pure $ StackOutput code outStr errStr
  case result of
    Left (err :: SomeException) -> pure $ StackOutput 127 ""
      ("stack-mcp: failed to run stack: " <> T.pack (show err))
    Right so -> pure so
 where
  -- Read a handle to Text line-by-line, accumulating into an IORef.
  -- When the handle is closed from the outside the read throws, and we stop.
  readHandle :: Handle -> IORef Text -> IO ()
  readHandle h ref = go
    where
      go = do
        r <- try (TIO.hGetLine h)
        case (r :: Either SomeException Text) of
          Left _    -> pure ()
          Right ln  -> do
            modifyIORef' ref (\acc -> if T.null acc then ln else acc <> "\n" <> ln)
            go

  closeSilent :: Handle -> IO ()
  closeSilent h = hClose h `catch` (\(_ :: SomeException) -> pure ())
