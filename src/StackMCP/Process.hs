{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StackMCP.Process
  ( runStackRaw
  , runStackBuild
  , ProcessConfig(..)
  , buildProcessConfig
  , defaultProcessConfig
  , StackOutput(..)
  , defaultTimeoutUs
  ) where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar, tryTakeMVar, threadDelay)
import Control.Exception (SomeException, try)
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import System.IO (Handle, hClose, hWaitForInput)
import System.Process
  (proc, createProcess, waitForProcess, terminateProcess,
   CreateProcess(..), StdStream(..))
import System.Exit (ExitCode(..))
import System.Timeout (timeout)

-- | Default timeout for stack commands: 5 minutes (in microseconds).
defaultTimeoutUs :: Int
defaultTimeoutUs = 5 * 60 * 1000000

-- | Configuration for process execution behavior.
data ProcessConfig = ProcessConfig
  { pcActivityTimeout :: !(Maybe Int)
    -- ^ When @Just us@, enable activity-based timeout: the timer resets every
    --   time stdout or stderr produces output.  When @Nothing@, no timeout.
  , pcDetectLock      :: !Bool
    -- ^ When @True@, detect lock-contention messages on stderr and early-exit
    --   with exit code 125.
  } deriving (Show)

-- | No timeout, no lock detection.  Used by exec, run, and background tasks.
defaultProcessConfig :: ProcessConfig
defaultProcessConfig = ProcessConfig Nothing False

-- | Activity-based 5-minute timeout with lock detection.
--   Used by build, test, bench, haddock, install, typecheck, pipeline, deps.
buildProcessConfig :: ProcessConfig
buildProcessConfig = ProcessConfig (Just defaultTimeoutUs) True

-- | Structured output from a stack command.
data StackOutput = StackOutput
  { soExitCode :: !Int
  , soStdout   :: !Text
  , soStderr   :: !Text
  } deriving (Show)

-- | Check whether a stderr line indicates lock contention.
isLockBlocked :: Text -> Bool
isLockBlocked ln = any (`T.isInfixOf` lower) patterns
  where
    lower = T.toLower ln
    patterns =
      [ "waiting for process lock"
      , "could not obtain lock"
      , "another copy of stack is running"
      , "lock is held by"
      , "lock file"
      , "resource busy"
      ]

-- | Run a stack command with no timeout and no lock detection.
--   Suitable for exec, run, ghci, and background tasks.
--   Passes --color=never to suppress ANSI escape codes.
--   Returns a synthetic exit code 127 if the process cannot be started.
--
--   On Windows, child processes spawned by @stack@ can inherit pipe handles.
--   Even after @stack@ exits, those children may hold the handles open.
--   Both @hGetContents@ and @hGetLine@ block while holding the Handle lock, so
--   @hClose@ from another thread also blocks.  We avoid this by using
--   @hWaitForInput@ with a short poll interval: the reader only calls
--   @hGetLine@ when data is known to be available, and checks a stop flag
--   between polls.  This ensures the reader exits promptly once signalled.
runStackRaw :: Maybe FilePath -> [Text] -> IO StackOutput
runStackRaw = runStackWithConfig defaultProcessConfig

-- | Run a stack command with build-appropriate settings: activity-based
--   timeout and lock-contention detection.
runStackBuild :: Maybe FilePath -> [Text] -> IO StackOutput
runStackBuild = runStackWithConfig buildProcessConfig

-- | Run a stack command with the given process configuration.
runStackWithConfig :: ProcessConfig -> Maybe FilePath -> [Text] -> IO StackOutput
runStackWithConfig cfg mcwd args = do
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
    -- Activity tracking: updated by readers on each line of output.
    activityRef <- newIORef =<< getCurrentTime
    -- Lock detection: set to True by stderr reader when lock pattern found.
    lockRef <- newIORef False
    let onLine = Just activityRef
        onLock = if pcDetectLock cfg then Just lockRef else Nothing
    outVar  <- newEmptyMVar
    errVar  <- newEmptyMVar
    _ <- forkIO (readHandle hout outRef stopRef onLine Nothing  >>= putMVar outVar)
    _ <- forkIO (readHandle herr errRef stopRef onLine onLock   >>= putMVar errVar)
    -- Wait for the process in a separate thread.
    exitVar <- newEmptyMVar
    _ <- forkIO (waitForProcess ph >>= putMVar exitVar)
    -- Choose wait strategy based on config.
    mExit <- case pcActivityTimeout cfg of
      Nothing       -> Just . Right <$> takeMVar exitVar
      Just timeoutUs -> activityWaitLoop exitVar activityRef lockRef timeoutUs
    -- Signal readers to stop, then collect results.
    writeIORef stopRef True
    case mExit of
      Nothing -> do
        terminateProcess ph
        _ <- timeout 3000000 (takeMVar outVar)
        _ <- timeout 3000000 (takeMVar errVar)
        outStr <- readIORef outRef
        errStr <- readIORef errRef
        pure $ StackOutput 124 outStr
          ("stack-mcp: command timed out (no output for "
           <> T.pack (show (timeoutMinutes cfg)) <> " minutes): stack "
           <> T.unwords args <> if T.null errStr then "" else "\n" <> errStr)
      Just (Left lockMsg) -> do
        terminateProcess ph
        _ <- timeout 3000000 (takeMVar outVar)
        _ <- timeout 3000000 (takeMVar errVar)
        outStr <- readIORef outRef
        errStr <- readIORef errRef
        pure $ StackOutput 125 outStr
          ("stack-mcp: project is locked by another process. "
           <> "Close other stack instances and retry.\n"
           <> "Detected: " <> lockMsg
           <> if T.null errStr then "" else "\n" <> errStr)
      Just (Right ec) -> do
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
  timeoutMinutes :: ProcessConfig -> Int
  timeoutMinutes c = case pcActivityTimeout c of
    Just us -> us `div` 60000000
    Nothing -> 0

  -- | Poll loop that resets the timeout every time output activity is seen.
  --   Returns:
  --     Nothing           — timed out (no activity for the given duration)
  --     Just (Left msg)   — lock contention detected (msg is the offending line)
  --     Just (Right ec)   — process exited normally
  activityWaitLoop
    :: MVar ExitCode -> IORef UTCTime -> IORef Bool -> Int
    -> IO (Maybe (Either Text ExitCode))
  activityWaitLoop exitVar activityRef lockRef timeoutUs = go
    where
      timeoutSec :: Double
      timeoutSec = fromIntegral timeoutUs / 1000000.0

      go = do
        mExit <- tryTakeMVar exitVar
        case mExit of
          Just ec -> pure (Just (Right ec))
          Nothing -> do
            -- Check for lock contention
            locked <- readIORef lockRef
            if locked
              then pure (Just (Left "lock contention detected on stderr"))
              else do
                -- Check activity timeout
                now     <- getCurrentTime
                lastAct <- readIORef activityRef
                let elapsed = realToFrac (diffUTCTime now lastAct) :: Double
                if elapsed >= timeoutSec
                  then pure Nothing  -- timed out
                  else threadDelay 500000 >> go

  -- | Read a handle using a non-blocking poll loop.
  --   @hWaitForInput@ with a 500ms timeout avoids holding the Handle lock
  --   for extended periods, so the reader can check the stop flag regularly
  --   and exit promptly when signalled.
  --
  --   When @mActivityRef@ is @Just@, updates the timestamp on each line read.
  --   When @mLockRef@ is @Just@, checks each line for lock-contention patterns.
  readHandle
    :: Handle -> IORef Text -> IORef Bool
    -> Maybe (IORef UTCTime) -> Maybe (IORef Bool)
    -> IO ()
  readHandle h ref stopRef mActivityRef mLockRef = go
    where
      go = do
        stop <- readIORef stopRef
        if stop
          then drain
          else do
            ready <- try (hWaitForInput h 500)
            case (ready :: Either SomeException Bool) of
              Left _      -> pure ()
              Right False -> go
              Right True  -> do
                r <- try (TIO.hGetLine h)
                case (r :: Either SomeException Text) of
                  Left _   -> pure ()
                  Right ln -> do
                    appendLine ref ln
                    signalActivity mActivityRef
                    checkLock mLockRef ln
                    go

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
                signalActivity mActivityRef
                checkLock mLockRef ln
                drain

  signalActivity :: Maybe (IORef UTCTime) -> IO ()
  signalActivity Nothing    = pure ()
  signalActivity (Just ref) = writeIORef ref =<< getCurrentTime

  checkLock :: Maybe (IORef Bool) -> Text -> IO ()
  checkLock Nothing    _  = pure ()
  checkLock (Just ref) ln = when (isLockBlocked ln) (writeIORef ref True)

  when :: Bool -> IO () -> IO ()
  when True  a = a
  when False _ = pure ()

  appendLine :: IORef Text -> Text -> IO ()
  appendLine ref ln =
    modifyIORef' ref (\acc -> if T.null acc then ln else acc <> "\n" <> ln)
