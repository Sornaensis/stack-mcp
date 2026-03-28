{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module StackMCP.Types where

import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)

-- | JSON-RPC 2.0 request
data JsonRpcRequest = JsonRpcRequest
  { rpcReqJsonrpc :: Text
  , rpcReqId      :: Maybe Value
  , rpcReqMethod  :: Text
  , rpcReqParams  :: Maybe Value
  } deriving (Show, Generic)

instance FromJSON JsonRpcRequest where
  parseJSON = withObject "JsonRpcRequest" $ \v ->
    JsonRpcRequest
      <$> v .: "jsonrpc"
      <*> v .:? "id"
      <*> v .: "method"
      <*> v .:? "params"

-- | JSON-RPC 2.0 response
data JsonRpcResponse = JsonRpcResponse
  { rpcResJsonrpc :: Text
  , rpcResId      :: Maybe Value
  , rpcResResult  :: Maybe Value
  , rpcResError   :: Maybe JsonRpcError
  } deriving (Show, Generic)

instance ToJSON JsonRpcResponse where
  toJSON r = object $
    [ "jsonrpc" .= rpcResJsonrpc r
    , "id"      .= rpcResId r
    ] ++ maybe [] (\v -> ["result" .= v]) (rpcResResult r)
      ++ maybe [] (\e -> ["error"  .= e]) (rpcResError r)

data JsonRpcError = JsonRpcError
  { errCode    :: Int
  , errMessage :: Text
  , errData    :: Maybe Value
  } deriving (Show, Generic)

instance ToJSON JsonRpcError where
  toJSON e = object $
    [ "code"    .= errCode e
    , "message" .= errMessage e
    ] ++ maybe [] (\d -> ["data" .= d]) (errData e)

-- | MCP Tool definition
data ToolDef = ToolDef
  { toolName        :: Text
  , toolDescription :: Text
  , toolInputSchema :: Value
  } deriving (Show, Generic)

instance ToJSON ToolDef where
  toJSON t = object
    [ "name"        .= toolName t
    , "description" .= toolDescription t
    , "inputSchema" .= toolInputSchema t
    ]

-- | MCP content block returned from tool calls
data ContentBlock = ContentBlock
  { contentType :: Text
  , contentText :: Text
  } deriving (Show, Generic)

instance ToJSON ContentBlock where
  toJSON c = object
    [ "type" .= contentType c
    , "text" .= contentText c
    ]

-- | Result of a tool call
data ToolResult = ToolResult
  { toolResultContent :: [ContentBlock]
  , toolResultIsError :: Bool
  } deriving (Show, Generic)

instance ToJSON ToolResult where
  toJSON r = object
    [ "content" .= toolResultContent r
    , "isError" .= toolResultIsError r
    ]

------------------------------------------------------------------------
-- MCP Resource types
------------------------------------------------------------------------

-- | MCP Resource definition
data ResourceDef = ResourceDef
  { resourceUri         :: Text
  , resourceName        :: Text
  , resourceDescription :: Maybe Text
  , resourceMimeType    :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON ResourceDef where
  toJSON r = object $
    [ "uri"  .= resourceUri r
    , "name" .= resourceName r
    ] ++ maybe [] (\d -> ["description" .= d]) (resourceDescription r)
      ++ maybe [] (\m -> ["mimeType" .= m]) (resourceMimeType r)

-- | MCP Resource content
data ResourceContent = ResourceContent
  { rcUri      :: Text
  , rcMimeType :: Maybe Text
  , rcText     :: Text
  } deriving (Show, Generic)

instance ToJSON ResourceContent where
  toJSON r = object $
    [ "uri"  .= rcUri r
    , "text" .= rcText r
    ] ++ maybe [] (\m -> ["mimeType" .= m]) (rcMimeType r)

------------------------------------------------------------------------
-- MCP Prompt types
------------------------------------------------------------------------

-- | MCP Prompt definition
data PromptDef = PromptDef
  { promptName        :: Text
  , promptDescription :: Maybe Text
  , promptArguments   :: [PromptArgument]
  } deriving (Show, Generic)

data PromptArgument = PromptArgument
  { paName        :: Text
  , paDescription :: Maybe Text
  , paRequired    :: Bool
  } deriving (Show, Generic)

instance ToJSON PromptDef where
  toJSON p = object $
    [ "name" .= promptName p
    ] ++ maybe [] (\d -> ["description" .= d]) (promptDescription p)
      ++ ["arguments" .= promptArguments p | not (null (promptArguments p))]

instance ToJSON PromptArgument where
  toJSON a = object $
    [ "name"     .= paName a
    , "required" .= paRequired a
    ] ++ maybe [] (\d -> ["description" .= d]) (paDescription a)

-- | MCP Prompt message
data PromptMessage = PromptMessage
  { pmRole    :: Text
  , pmContent :: ContentBlock
  } deriving (Show, Generic)

instance ToJSON PromptMessage where
  toJSON m = object
    [ "role"    .= pmRole m
    , "content" .= pmContent m
    ]

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

mkSuccess :: Maybe Value -> Value -> JsonRpcResponse
mkSuccess reqId result = JsonRpcResponse "2.0" reqId (Just result) Nothing

mkError :: Maybe Value -> Int -> Text -> JsonRpcResponse
mkError reqId code msg = JsonRpcResponse "2.0" reqId Nothing
  (Just (JsonRpcError code msg Nothing))

textContent :: Text -> ContentBlock
textContent = ContentBlock "text"

jsonContent :: Value -> ContentBlock
jsonContent v = ContentBlock "text" (encodeText v)
  where
    encodeText = TE.decodeUtf8 . BL.toStrict . encode

mkToolResult :: Text -> ToolResult
mkToolResult t = ToolResult [textContent t] False

mkToolResultJSON :: Value -> ToolResult
mkToolResultJSON v = ToolResult [jsonContent v] False

mkToolError :: Text -> ToolResult
mkToolError t = ToolResult [textContent t] True

mkToolErrorJSON :: Value -> ToolResult
mkToolErrorJSON v = ToolResult [jsonContent v] True

-- | Structured error with type, message, command info, and suggestions
mkStructuredError :: Text -> Text -> [Text] -> [Text] -> ToolResult
mkStructuredError errType message cmd suggestions =
  mkToolErrorJSON $ object
    [ "error_type"  .= errType
    , "message"     .= message
    , "command"     .= cmd
    , "suggestions" .= suggestions
    ]
