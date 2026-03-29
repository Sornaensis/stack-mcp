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
import System.IO (Handle, hClose, hSetBinaryMode, hWaitForInput)
import System.Process
  (proc, createProcess, waitForProcess, terminateProcess,
   CreateProcess(..), StdStream(..))
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
--
--   On Windows, child processes spawned by @stack@ can inherit pipe handles.
--   Even after @stack@ exits, those children may hold the handles open.
--   Both @hGetContents@ and @hGetLine@ block while holding the Handle lock, so
--   @hClose@ from another thread also blocks.  We avoid this by using
--   @hWaitForInput@ with a short poll interval: the reader only calls
--   @hGetLine@ when data is known to be available, and checks a stop flag
--   between polls.  This ensures the reader exits promptly once signalled.
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
    -- Accumulate output incrementally.
    outRef  <- newIORef T.empty
    errRef  <- newIORef T.empty
    stopRef <- newIORef False
    outVar  <- newEmptyMVar
    errVar  <- newEmptyMVar
    _ <- forkIO (readHandle hout outRef stopRef >>= putMVar outVar)
    _ <- forkIO (readHandle herr errRef stopRef >>= putMVar errVar)
    -- Wait for the process in a separate thread so the timeout is always
    -- interruptible (takeMVar can receive async exceptions).
    exitVar <- newEmptyMVar
    _ <- forkIO (waitForProcess ph >>= putMVar exitVar)
    mExit <- timeout defaultTimeoutUs (takeMVar exitVar)
    -- Signal readers to stop, then collect results.
    writeIORef stopRef True
    case mExit of
      Nothing -> do
        terminateProcess ph
        -- Give readers up to 3s to notice the stop flag and finish.
        _ <- timeout 3000000 (takeMVar outVar)
        _ <- timeout 3000000 (takeMVar errVar)
        outStr <- readIORef outRef
        errStr <- readIORef errRef
        pure $ StackOutput 124 outStr
          ("stack-mcp: command timed out after 5 minutes: stack "
           <> T.unwords args <> if T.null errStr then "" else "\n" <> errStr)
      Just ec -> do
        -- Give readers up to 3s to drain remaining data and notice the flag.
        _ <- timeout 3000000 (takeMVar outVar)
        _ <- timeout 3000000 (takeMVar errVar)
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
  -- | Read a handle using a non-blocking poll loop.
  --   @hWaitForInput@ with a 500ms timeout avoids holding the Handle lock
  --   for extended periods, so the reader can check the stop flag regularly
  --   and exit promptly when signalled.
  readHandle :: Handle -> IORef Text -> IORef Bool -> IO ()
  readHandle h ref stopRef = go
    where
      go = do
        stop <- readIORef stopRef
        if stop
          then drain  -- drain any remaining buffered data before exiting
          else do
            ready <- try (hWaitForInput h 500)
            case (ready :: Either SomeException Bool) of
              Left _      -> pure ()  -- handle closed or error
              Right False -> go       -- no data yet, poll again
              Right True  -> do
                r <- try (TIO.hGetLine h)
                case (r :: Either SomeException Text) of
                  Left _   -> pure ()
                  Right ln -> do
                    appendLine ref ln
                    go

      -- After stop is signalled, read whatever is immediately available.
      drain = do
        ready <- try (hWaitForInput h 0)
        case (ready :: Either SomeException Bool) of
          Left _      -> pure ()
          Right False -> pure ()
          Right True  -> do
            r <- try (TIO.hGetLine h)
            case (r :: Either SomeException Text) of
              Left _   -> pure ()
              Right ln -> do
                appendLine ref ln
                drain

  appendLine :: IORef Text -> Text -> IO ()
  appendLine ref ln =
    modifyIORef' ref (\acc -> if T.null acc then ln else acc <> "\n" <> ln)
