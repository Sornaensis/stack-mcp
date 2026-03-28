{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StackMCP.Server
  ( runServer
  ) where

import Control.Concurrent (MVar, newMVar, newEmptyMVar, putMVar, takeMVar, withMVar, modifyMVar_, forkIO, myThreadId, ThreadId, killThread)
import Control.Exception (SomeException, AsyncException, catch, try, fromException)
import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import System.IO (hFlush, stdout, stdin, stderr, hPutStrLn, hSetBuffering, BufferMode(..))
import StackMCP.Types
import StackMCP.Tools (allTools, callTool)
import StackMCP.Resources (listResources, readResource)
import StackMCP.Prompts (allPrompts, getPrompt)
import StackMCP.TaskManager (TaskManager, newTaskManager)

------------------------------------------------------------------------
-- Server state
------------------------------------------------------------------------

data TransportMode = NDJSON | ContentLength deriving (Eq)

data ServerState = ServerState
  { ssCwd       :: IORef (Maybe FilePath)
  , ssLock      :: MVar ()
  , ssActive    :: MVar (Map.Map Text ThreadId)
  , ssTasks     :: TaskManager
  , ssTransport :: IORef TransportMode
  }

------------------------------------------------------------------------
-- Main entry point
------------------------------------------------------------------------

-- | Run the MCP server on stdio.
runServer :: IO ()
runServer = do
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr LineBuffering
  hPutStrLn stderr "stack-mcp: server starting on stdio (v0.2.0)"
  st <- ServerState
    <$> newIORef Nothing
    <*> newMVar ()
    <*> newMVar Map.empty
    <*> newTaskManager
    <*> newIORef ContentLength  -- default; updated on first message
  loop st
  where
    loop st = do
      mResult <- readMessage
      case mResult of
        Nothing -> pure () -- EOF
        Just (mode, line) -> do
          writeIORef (ssTransport st) mode
          case eitherDecode (BL.fromStrict line) of
            Left err -> safeSend st $
              mkError Nothing (-32700) (T.pack $ "Parse error: " <> err)
            Right req -> handleDispatch st req
          loop st

------------------------------------------------------------------------
-- Request dispatch
------------------------------------------------------------------------

handleDispatch :: ServerState -> JsonRpcRequest -> IO ()
handleDispatch st req = case rpcReqMethod req of
  -- Notifications (no response)
  "notifications/initialized" -> pure ()
  "notifications/cancelled"   -> handleCancelled st req
  "logging/setLevel"          -> safeSend st $ mkSuccess (rpcReqId req) (object [])

  -- Tool calls: fork for cancellation support
  "tools/call" -> do
    let rid      = reqIdText (rpcReqId req)
        tName    = extractToolName req
    barrier <- newEmptyMVar  -- ensures thread registers before we return
    _ <- forkIO $ do
      myTid <- myThreadId
      modifyMVar_ (ssActive st) (pure . Map.insert rid myTid)
      putMVar barrier ()  -- signal: registered
      let tryLog msg = try (sendLog st "info" msg)
                         :: IO (Either SomeException ())
          run = do
            _ <- tryLog ("Executing: " <> tName)
            r <- handleToolsCall (ssCwd st) (ssTasks st) req
            _ <- tryLog ("Completed: " <> tName)
            modifyMVar_ (ssActive st) (pure . Map.delete rid)
            safeSend st r
          onErr (e :: SomeException) = do
            modifyMVar_ (ssActive st) (pure . Map.delete rid)
              `catch` (\(e2 :: SomeException) ->
                hPutStrLn stderr $ "stack-mcp: warning: failed to unregister request " ++ show rid ++ ": " ++ show e2)
            let resp = case fromException e :: Maybe AsyncException of
                  Just _  -> mkError (rpcReqId req) (-32800) "Request cancelled"
                  Nothing -> mkError (rpcReqId req) (-32603)
                               ("Internal error: " <> T.pack (show e))
            safeSend st resp
              `catch` (\(e2 :: SomeException) ->
                hPutStrLn stderr $ "stack-mcp: warning: failed to send error response: " ++ show e2)
      run `catch` onErr
    takeMVar barrier  -- wait for thread to register
    pure ()

  -- Synchronous handlers
  "initialize"      -> safeSend st =<< handleInitialize req
  "tools/list"      -> safeSend st =<< handleToolsList req
  "resources/list"  -> safeSend st =<< handleResourcesList (ssCwd st) req
  "resources/read"  -> safeSend st =<< handleResourcesRead (ssCwd st) req
  "prompts/list"    -> safeSend st =<< handlePromptsList req
  "prompts/get"     -> safeSend st =<< handlePromptsGet req
  "ping"            -> safeSend st $ mkSuccess (rpcReqId req) (object [])
  method            -> safeSend st $ mkError (rpcReqId req) (-32601)
                         ("Method not found: " <> method)

------------------------------------------------------------------------
-- Cancellation
------------------------------------------------------------------------

handleCancelled :: ServerState -> JsonRpcRequest -> IO ()
handleCancelled st req = do
  let rid = case rpcReqParams req of
              Just (Object o) -> case KM.lookup (fromText "requestId") o of
                Just (String s) -> s
                Just (Number n) -> T.pack (show (round n :: Int))
                _               -> ""
              _ -> ""
  if T.null rid
    then pure ()
    else do
      mTid <- withMVar (ssActive st) (pure . Map.lookup rid)
      case mTid of
        Just tid -> do
          sendLog st "info" ("Cancelling request: " <> rid)
          killThread tid
        Nothing -> pure ()

------------------------------------------------------------------------
-- Initialize
------------------------------------------------------------------------

handleInitialize :: JsonRpcRequest -> IO JsonRpcResponse
handleInitialize req = pure $ mkSuccess (rpcReqId req) $ object
  [ "protocolVersion" .= ("2024-11-05" :: Text)
  , "capabilities" .= object
      [ "tools" .= object
          [ "listChanged" .= False
          ]
      , "resources" .= object
          [ "subscribe"   .= False
          , "listChanged" .= False
          ]
      , "prompts" .= object
          [ "listChanged" .= False
          ]
      , "logging" .= object ([] :: [(Key, Value)])
      ]
  , "serverInfo" .= object
      [ "name"    .= ("stack-mcp" :: Text)
      , "version" .= ("0.2.0" :: Text)
      ]
  ]

------------------------------------------------------------------------
-- Tools
------------------------------------------------------------------------

handleToolsList :: JsonRpcRequest -> IO JsonRpcResponse
handleToolsList req = pure $ mkSuccess (rpcReqId req) $ object
  [ "tools" .= allTools
  ]

handleToolsCall :: IORef (Maybe FilePath) -> TaskManager -> JsonRpcRequest -> IO JsonRpcResponse
handleToolsCall cwdRef tm req = do
  case rpcReqParams req of
    Just (Object params) -> do
      let mName = case KM.lookup (fromText "name") params of
            Just (String n) -> Just n
            _               -> Nothing
          mArgs = KM.lookup (fromText "arguments") params
      case mName of
        Nothing -> pure $ mkError (rpcReqId req) (-32602) "Missing tool name"
        Just name -> do
          let args = maybe (object []) id mArgs
          result <- callTool cwdRef tm name args
          pure $ mkSuccess (rpcReqId req) (toJSON result)
    _ -> pure $ mkError (rpcReqId req) (-32602) "Invalid params"

------------------------------------------------------------------------
-- Resources
------------------------------------------------------------------------

handleResourcesList :: IORef (Maybe FilePath) -> JsonRpcRequest -> IO JsonRpcResponse
handleResourcesList cwdRef req = do
  resources <- listResources cwdRef
  pure $ mkSuccess (rpcReqId req) $ object
    [ "resources" .= resources ]

handleResourcesRead :: IORef (Maybe FilePath) -> JsonRpcRequest -> IO JsonRpcResponse
handleResourcesRead cwdRef req = do
  let uri = case rpcReqParams req of
              Just (Object o) -> case KM.lookup (fromText "uri") o of
                Just (String u) -> u
                _               -> ""
              _ -> ""
  if T.null uri
    then pure $ mkError (rpcReqId req) (-32602) "Missing uri parameter"
    else do
      result <- readResource cwdRef uri
      case result of
        Right content -> pure $ mkSuccess (rpcReqId req) $ object
          [ "contents" .= [content] ]
        Left err -> pure $ mkError (rpcReqId req) (-32602) err

------------------------------------------------------------------------
-- Prompts
------------------------------------------------------------------------

handlePromptsList :: JsonRpcRequest -> IO JsonRpcResponse
handlePromptsList req = pure $ mkSuccess (rpcReqId req) $ object
  [ "prompts" .= allPrompts ]

handlePromptsGet :: JsonRpcRequest -> IO JsonRpcResponse
handlePromptsGet req = do
  let (mName, args) = case rpcReqParams req of
        Just (Object o) ->
          ( case KM.lookup (fromText "name") o of
              Just (String n) -> Just n
              _               -> Nothing
          , case KM.lookup (fromText "arguments") o of
              Just v  -> v
              Nothing -> object []
          )
        _ -> (Nothing, object [])
  case mName of
    Nothing -> pure $ mkError (rpcReqId req) (-32602) "Missing prompt name"
    Just name -> case getPrompt name args of
      Right msgs -> pure $ mkSuccess (rpcReqId req) $ object
        [ "description" .= name
        , "messages" .= msgs
        ]
      Left err -> pure $ mkError (rpcReqId req) (-32602) err

------------------------------------------------------------------------
-- Logging / notifications
------------------------------------------------------------------------

-- | Send a log notification to the client.
sendLog :: ServerState -> Text -> Text -> IO ()
sendLog st level message = sendNotification st "notifications/message" $ object
  [ "level"  .= level
  , "logger" .= ("stack-mcp" :: Text)
  , "data"   .= message
  ]

-- | Send a JSON-RPC notification (no id).
sendNotification :: ServerState -> Text -> Value -> IO ()
sendNotification st method params = withMVar (ssLock st) $ \_ -> do
  let notif = object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "method"  .= method
        , "params"  .= params
        ]
  mode <- readIORef (ssTransport st)
  sendRaw mode (BL.toStrict (encode notif))

------------------------------------------------------------------------
-- Transport
------------------------------------------------------------------------

-- | Send a JSON-RPC response with write lock.
safeSend :: ServerState -> JsonRpcResponse -> IO ()
safeSend st resp = withMVar (ssLock st) $ \_ -> do
  mode <- readIORef (ssTransport st)
  sendRaw mode (BL.toStrict (encode resp))

-- | Write bytes to stdout in the appropriate framing.
sendRaw :: TransportMode -> BS.ByteString -> IO ()
sendRaw ContentLength body = do
  let header = TE.encodeUtf8 $ "Content-Length: " <> T.pack (show (BS.length body)) <> "\r\n\r\n"
  BS.hPut stdout header
  BS.hPut stdout body
  hFlush stdout
sendRaw NDJSON body = do
  BS.hPut stdout body
  BS.hPut stdout "\n"
  hFlush stdout

-- | Read a JSON-RPC message, auto-detecting the framing.
--   Returns the detected transport mode and the message bytes.
readMessage :: IO (Maybe (TransportMode, BS.ByteString))
readMessage = do
  c <- skipWhitespaceAndPeek
  case c of
    Nothing  -> pure Nothing
    Just '{' -> fmap (\bs -> (NDJSON, bs)) <$> readNDJSON
    _        -> fmap (\bs -> (ContentLength, bs)) <$> readFramed

-- | Skip whitespace bytes and peek at the next non-whitespace byte.
skipWhitespaceAndPeek :: IO (Maybe Char)
skipWhitespaceAndPeek = do
  result <- try (BS.hGet stdin 1) :: IO (Either SomeException BS.ByteString)
  case result of
    Left _   -> pure Nothing
    Right bs
      | BS.null bs -> pure Nothing
      | otherwise  ->
          let c = BS8.head bs
          in if c == ' ' || c == '\n' || c == '\r' || c == '\t'
             then skipWhitespaceAndPeek
             else pure (Just c)

-- | Read a newline-delimited JSON message (first '{' already consumed).
readNDJSON :: IO (Maybe BS.ByteString)
readNDJSON = do
  result <- try (BS8.hGetLine stdin) :: IO (Either SomeException BS.ByteString)
  case result of
    Left _   -> pure Nothing
    Right rest ->
      let line = BS.cons (fromIntegral (fromEnum '{')) rest
          trimmed = if not (BS.null line) && BS8.last line == '\r'
                    then BS.init line
                    else line
      in if BS.null trimmed
         then pure Nothing
         else pure (Just trimmed)

-- | Read a Content-Length framed message (first header byte already consumed).
readFramed :: IO (Maybe BS.ByteString)
readFramed = do
  -- We already consumed one byte — read the rest of this header line
  result <- try (BS8.hGetLine stdin) :: IO (Either SomeException BS.ByteString)
  case result of
    Left _ -> pure Nothing
    Right restOfLine -> do
      let firstHeader = BS.cons (fromIntegral (fromEnum 'C')) restOfLine
      moreHeaders <- readRemainingHeaders [firstHeader]
      case findContentLength moreHeaders of
        Nothing  -> pure Nothing
        Just len
          | len <= 0       -> pure Nothing
          | len > 10485760 -> pure Nothing  -- reject >10MB messages
          | otherwise      -> readExactly len

-- | Read remaining headers after the first, until blank line.
readRemainingHeaders :: [BS.ByteString] -> IO [BS.ByteString]
readRemainingHeaders acc = go 0 acc
  where
    maxHeaders = 256 :: Int
    go n hdrs
      | n >= maxHeaders = pure hdrs
      | otherwise = do
          line <- BS8.hGetLine stdin
          if BS.null line || line == "\r"
            then pure hdrs
            else go (n + 1) (line : hdrs)

-- | Read exactly @n@ bytes from stdin, retrying on partial reads.
readExactly :: Int -> IO (Maybe BS.ByteString)
readExactly n = go [] n
  where
    go acc remaining
      | remaining <= 0 = pure (Just (BS.concat (reverse acc)))
      | otherwise = do
          chunk <- BS.hGet stdin remaining
          if BS.null chunk
            then pure Nothing  -- EOF before full message
            else go (chunk : acc) (remaining - BS.length chunk)

findContentLength :: [BS.ByteString] -> Maybe Int
findContentLength [] = Nothing
findContentLength (h:hs) =
  case parseContentLength h of
    Just n  -> Just n
    Nothing -> findContentLength hs

parseContentLength :: BS.ByteString -> Maybe Int
parseContentLength bs =
  let t = TE.decodeUtf8 bs
  in if "Content-Length:" `T.isPrefixOf` (T.strip t)
     then case reads (T.unpack (T.strip (T.drop 15 (T.strip t)))) of
            [(n, "")] -> Just n
            _         -> Nothing
     else Nothing

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

reqIdText :: Maybe Value -> Text
reqIdText (Just (String s)) = s
reqIdText (Just (Number n)) = T.pack (show (round n :: Int))
reqIdText _                 = ""

extractToolName :: JsonRpcRequest -> Text
extractToolName req = case rpcReqParams req of
  Just (Object o) -> case KM.lookup (fromText "name") o of
    Just (String n) -> n
    _               -> "unknown"
  _ -> "unknown"
