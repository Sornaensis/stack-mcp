{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StackMCP.TaskManager
  ( TaskManager
  , TaskId
  , TaskCommand(..)
  , TaskInfo(..)
  , TaskStatus(..)
  , SpawnError(..)
  , newTaskManager
  , spawnTask
  , readTaskOutput
  , writeAndRead
  , writeTaskStdin
  , killTask
  , listTasks
  , getTaskInfo
  , commandToDisplay
  ) where

import Control.Concurrent (MVar, newMVar, modifyMVar, modifyMVar_, withMVar, forkIO, threadDelay)
import Control.Exception (SomeException, try)
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import System.Exit (ExitCode(..))
import System.IO (Handle, hClose, hFlush, hIsEOF, hReady, hGetChar, hPutStr, hSetBinaryMode, hSetBuffering, BufferMode(..))
import System.Process
  ( CreateProcess(..)
  , StdStream(..)
  , ProcessHandle
  , proc
  , createProcess
  , getProcessExitCode
  , terminateProcess
  , waitForProcess
  )

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

type TaskId = Text

-- | Restricted set of commands that may be spawned as background tasks.
data TaskCommand
  = TaskRun  ![Text]        -- ^ @stack run [args]@
  | TaskExec !Text ![Text]  -- ^ @stack exec \<cmd\> [-- args]@
  | TaskGhci ![Text]        -- ^ @stack ghci [args]@
  deriving (Show, Eq)

data TaskStatus
  = Running
  | Exited !Int
  deriving (Show, Eq)

data SpawnError
  = SpawnFailed !Text  -- ^ Process could not be started
  deriving (Show, Eq)

data TaskState = TaskState
  { tsStdin    :: !Handle
  , tsStdout   :: !Handle
  , tsStderr   :: !Handle
  , tsProcess  :: !ProcessHandle
  , tsCommand  :: !Text         -- for display
  , tsBufOut   :: !(IORef Text) -- accumulated stdout
  , tsBufErr   :: !(IORef Text) -- accumulated stderr
  }

data TaskInfo = TaskInfo
  { tiId      :: !TaskId
  , tiCommand :: !Text
  , tiStatus  :: !TaskStatus
  } deriving (Show)

data TaskManager = TaskManager
  { tmTasks   :: !(MVar (Map TaskId TaskState))
  , tmNextId  :: !(IORef Int)
  }

------------------------------------------------------------------------
-- Command helpers
------------------------------------------------------------------------

-- | Convert a TaskCommand to the stack args list.
commandToArgs :: TaskCommand -> [String]
commandToArgs (TaskRun  args)      = "run"  : map T.unpack args
commandToArgs (TaskExec cmd args)  = "exec" : T.unpack cmd : "--" : map T.unpack args
commandToArgs (TaskGhci args)      = "ghci" : map T.unpack args

-- | Human-readable display string for a task command.
commandToDisplay :: TaskCommand -> Text
commandToDisplay (TaskRun  args)     = "stack run "  <> T.unwords args
commandToDisplay (TaskExec cmd args) = "stack exec " <> cmd <> " -- " <> T.unwords args
commandToDisplay (TaskGhci args)     = "stack ghci " <> T.unwords args

------------------------------------------------------------------------
-- Constructor
------------------------------------------------------------------------

newTaskManager :: IO TaskManager
newTaskManager = TaskManager
  <$> newMVar Map.empty
  <*> newIORef 1

------------------------------------------------------------------------
-- Spawn
------------------------------------------------------------------------

-- | Spawn a background stack process. Only 'TaskCommand' variants are accepted.
--   Returns either a spawn error or the new task ID.
--   Also cleans up any dead tasks from the map.
spawnTask :: TaskManager -> Maybe FilePath -> TaskCommand -> IO (Either SpawnError TaskId)
spawnTask tm mcwd tcmd = do
  cleanupDeadTasks tm
  let cp = (proc "stack" (commandToArgs tcmd))
        { cwd     = mcwd
        , std_in  = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }
  result <- try (createProcess cp) :: IO (Either SomeException (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle))
  case result of
    Left ex -> pure $ Left $ SpawnFailed (T.pack (show ex))
    Right (Just hIn, Just hOut, Just hErr, ph) -> do
      tid <- nextTaskId tm
      -- Set all handles to non-blocking text mode with line buffering
      mapM_ (\h -> hSetBinaryMode h False >> hSetBuffering h LineBuffering)
        [hIn, hOut, hErr]
      bufOut <- newIORef ""
      bufErr <- newIORef ""
      let ts = TaskState
            { tsStdin   = hIn
            , tsStdout  = hOut
            , tsStderr  = hErr
            , tsProcess = ph
            , tsCommand = commandToDisplay tcmd
            , tsBufOut  = bufOut
            , tsBufErr  = bufErr
            }
      -- Start reader threads that accumulate output into the buffers
      _ <- forkIO $ readerLoop hOut bufOut
      _ <- forkIO $ readerLoop hErr bufErr
      modifyMVar_ (tmTasks tm) (pure . Map.insert tid ts)
      pure $ Right tid
    Right _ -> pure $ Left $ SpawnFailed "Failed to create pipes for process"

-- | Background reader: reads available data from a handle into an IORef buffer.
readerLoop :: Handle -> IORef Text -> IO ()
readerLoop h buf = go
  where
    go = do
      result <- try (readAvailable h) :: IO (Either SomeException Text)
      case result of
        Left _    -> pure ()  -- handle closed or error → stop
        Right chunk
          | T.null chunk -> pure ()  -- EOF
          | otherwise    -> do
              atomicModifyIORef' buf (\old -> (old <> chunk, ()))
              go

-- | Read all currently available data from a handle.
readAvailable :: Handle -> IO Text
readAvailable h = do
  eof <- hIsEOF h
  if eof
    then pure ""
    else do
      -- Read character by character, collecting lines
      txt <- readAvailableChars h
      if T.null txt then pure "" else pure txt

-- | Read as many characters as are available from a handle.
readAvailableChars :: Handle -> IO Text
readAvailableChars h = go []
  where
    go acc = do
      ready <- hReady h
      if ready
        then do
          c <- hGetChar h
          go (c : acc)
        else
          -- If nothing's ready but we haven't read anything yet,
          -- block for one char so the reader thread doesn't spin.
          if null acc
            then do
              eof <- hIsEOF h
              if eof then pure "" else do
                c <- hGetChar h
                go [c]  -- got one, now try non-blocking for more
            else pure (T.pack (reverse acc))

------------------------------------------------------------------------
-- Read output
------------------------------------------------------------------------

-- | Read and drain accumulated output from a task.
--   Returns (stdout_chunk, stderr_chunk).
readTaskOutput :: TaskManager -> TaskId -> IO (Maybe (Text, Text, TaskStatus))
readTaskOutput tm tid = do
  mts <- withMVar (tmTasks tm) (pure . Map.lookup tid)
  case mts of
    Nothing -> pure Nothing
    Just ts -> do
      out <- atomicModifyIORef' (tsBufOut ts) (\b -> ("", b))
      err <- atomicModifyIORef' (tsBufErr ts) (\b -> ("", b))
      status <- getStatus ts
      pure (Just (out, err, status))

-- | Write a line to a task's stdin, wait for output, and return the response.
--   Drains any stale buffer first so the result is only from this expression.
--
--   If @sentinel@ is non-empty, accumulates stdout across retries until the
--   sentinel text appears (deterministic completion). If empty, returns as
--   soon as any output is available (best-effort).
--
--   Retries up to @maxRetries@ times with @delayUs@ microseconds between each.
writeAndRead :: TaskManager -> TaskId -> Text -> Text -> Int -> Int -> IO (Either Text (Text, Text, TaskStatus))
writeAndRead tm tid input sentinel delayUs maxRetries = do
  mts <- withMVar (tmTasks tm) (pure . Map.lookup tid)
  case mts of
    Nothing -> pure $ Left "Task not found"
    Just ts -> do
      -- Drain stale output
      _ <- atomicModifyIORef' (tsBufOut ts) (\_ -> ("", ()))
      _ <- atomicModifyIORef' (tsBufErr ts) (\_ -> ("", ()))
      -- Write input (caller is responsible for trailing newlines)
      wResult <- try $ do
        hPutStr (tsStdin ts) (T.unpack input)
        hFlush (tsStdin ts)
      case wResult of
        Left (e :: SomeException) -> pure $ Left ("Write failed: " <> T.pack (show e))
        Right _ -> accumLoop ts "" "" 0
  where
    accumLoop ts accOut accErr attempt = do
      threadDelay delayUs
      out <- atomicModifyIORef' (tsBufOut ts) (\b -> ("", b))
      err <- atomicModifyIORef' (tsBufErr ts) (\b -> ("", b))
      status <- getStatus ts
      let newOut = accOut <> out
          newErr = accErr <> err
          done
            | status /= Running    = True
            | attempt >= maxRetries = True
            | T.null sentinel       = not (T.null newOut) || not (T.null newErr)
            | otherwise             = sentinel `T.isInfixOf` newOut
      if done
        then pure $ Right (newOut, newErr, status)
        else accumLoop ts newOut newErr (attempt + 1)

------------------------------------------------------------------------
-- Write to stdin
------------------------------------------------------------------------

-- | Write text to a task's stdin.
writeTaskStdin :: TaskManager -> TaskId -> Text -> IO (Maybe Text)
writeTaskStdin tm tid input = do
  mts <- withMVar (tmTasks tm) (pure . Map.lookup tid)
  case mts of
    Nothing -> pure (Just "Task not found")
    Just ts -> do
      result <- try $ do
        let h = tsStdin ts
        hPutStr h (T.unpack input)
        hFlush h
      case result of
        Left (e :: SomeException) -> pure (Just ("Write failed: " <> T.pack (show e)))
        Right _ -> pure Nothing

------------------------------------------------------------------------
-- Kill
------------------------------------------------------------------------

-- | Terminate a running task.
killTask :: TaskManager -> TaskId -> IO (Maybe Text)
killTask tm tid = do
  mts <- modifyMVar (tmTasks tm) $ \m ->
    case Map.lookup tid m of
      Nothing -> pure (m, Nothing)
      Just ts -> pure (m, Just ts)  -- keep in map for final read
  case mts of
    Nothing -> pure (Just "Task not found")
    Just ts -> do
      _ <- try (terminateProcess (tsProcess ts)) :: IO (Either SomeException ())
      -- Wait briefly for cleanup, then close handles
      _ <- forkIO $ do
        _ <- try (waitForProcess (tsProcess ts)) :: IO (Either SomeException ExitCode)
        _ <- try (hClose (tsStdin ts)) :: IO (Either SomeException ())
        pure ()
      pure Nothing

------------------------------------------------------------------------
-- List / Info
------------------------------------------------------------------------

-- | List all tasks with their current status.
--   Also removes dead tasks whose output has been fully drained.
listTasks :: TaskManager -> IO [TaskInfo]
listTasks tm = do
  cleanupDeadTasks tm
  tasks <- withMVar (tmTasks tm) pure
  mapM toInfo (Map.toList tasks)
  where
    toInfo (tid, ts) = do
      status <- getStatus ts
      pure TaskInfo
        { tiId      = tid
        , tiCommand = tsCommand ts
        , tiStatus  = status
        }

-- | Get info for a single task.
getTaskInfo :: TaskManager -> TaskId -> IO (Maybe TaskInfo)
getTaskInfo tm tid = do
  mts <- withMVar (tmTasks tm) (pure . Map.lookup tid)
  case mts of
    Nothing -> pure Nothing
    Just ts -> do
      status <- getStatus ts
      pure $ Just TaskInfo
        { tiId      = tid
        , tiCommand = tsCommand ts
        , tiStatus  = status
        }

------------------------------------------------------------------------
-- Cleanup
------------------------------------------------------------------------

-- | Remove dead tasks (exited processes with drained buffers) from the map.
cleanupDeadTasks :: TaskManager -> IO ()
cleanupDeadTasks tm = modifyMVar_ (tmTasks tm) $ \m -> do
  pairs <- mapM checkDead (Map.toList m)
  let alive = [p | (p, False) <- pairs]
      dead  = [ts | ((_, ts), True) <- pairs]
  -- Close handles for dead tasks
  mapM_ closeSilent dead
  pure (Map.fromList alive)
  where
    checkDead (tid, ts) = do
      status <- getStatus ts
      case status of
        Running -> pure ((tid, ts), False)
        Exited _ -> do
          -- Only remove if both buffers are empty (output has been read)
          out <- readIORef (tsBufOut ts)
          err <- readIORef (tsBufErr ts)
          pure ((tid, ts), T.null out && T.null err)
    closeSilent ts = do
      _ <- try (hClose (tsStdin ts)) :: IO (Either SomeException ())
      pure ()

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

getStatus :: TaskState -> IO TaskStatus
getStatus ts = do
  mExit <- getProcessExitCode (tsProcess ts)
  pure $ case mExit of
    Nothing               -> Running
    Just ExitSuccess      -> Exited 0
    Just (ExitFailure n)  -> Exited n

nextTaskId :: TaskManager -> IO TaskId
nextTaskId tm = do
  n <- atomicModifyIORef' (tmNextId tm) (\i -> (i + 1, i))
  pure $ "task-" <> T.pack (show n)
