{-# LANGUAGE OverloadedStrings #-}

module StackMCP.Tools.Common
  ( -- * Parameter extraction
    getParamText
  , getParamBool
  , getParamMaybeBool
  , getParamInt
  , getParamArray
    -- * Schema builders
  , mkSchema
  , strProp
  , boolProp
  , intProp
  , enumProp
  , arrayProp
    -- * Argument helpers
  , tagEach
    -- * Structured error helpers
  , mkCommandError
  , mkCommandErrorWithDiags
  , mkCommandErrorFiltered
    -- * Re-exports for tool modules
  , module Data.Aeson
  , Text
  , ToolDef(..)
  , ToolResult
  , mkToolResult
  , mkToolResultJSON
  , mkToolError
  , mkToolErrorJSON
  , mkStructuredError
  , runStackRaw
  , runStackBuild
  , StackOutput(..)
  ) where

import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap qualified as KM
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Text qualified as T
import StackMCP.Types (ToolDef(..), ToolResult, mkToolResult, mkToolResultJSON, mkToolError, mkToolErrorJSON, mkStructuredError)
import StackMCP.Process (runStackRaw, runStackBuild, StackOutput(..))
import StackMCP.Tools.Parse (GhcDiagnostic, parseGhcDiagnostics, diagnosticsSummary, filteredDiagnosticsSummary, DepError, parseDepErrors, depErrorsSummary)

------------------------------------------------------------------------
-- Parameter extraction
------------------------------------------------------------------------

getParamText :: Text -> Value -> Text
getParamText key val = case val of
  Object o -> case KM.lookup (fromText key) o of
    Just (String s) -> s
    _               -> ""
  _ -> ""

getParamBool :: Text -> Value -> Bool
getParamBool key val = case val of
  Object o -> case KM.lookup (fromText key) o of
    Just (Bool b) -> b
    _             -> False
  _ -> False

-- | Like getParamBool but distinguishes missing from explicitly false.
getParamMaybeBool :: Text -> Value -> Maybe Bool
getParamMaybeBool key val = case val of
  Object o -> case KM.lookup (fromText key) o of
    Just (Bool b) -> Just b
    _             -> Nothing
  _ -> Nothing

getParamInt :: Text -> Value -> Maybe Int
getParamInt key val = case val of
  Object o -> case KM.lookup (fromText key) o of
    Just (Number n) ->
      let i = truncate n :: Int
      in if fromIntegral i == n then Just i else Nothing
    _               -> Nothing
  _ -> Nothing

------------------------------------------------------------------------
-- Schema builders
------------------------------------------------------------------------

-- | Build a JSON Schema "object" with given properties and optional required list.
mkSchema :: [(Text, Value)] -> [Text] -> Value
mkSchema props req = object $
  [ "type" .= ("object" :: Text)
  , "properties" .= object [ fromText k .= v | (k, v) <- props ]
  ] ++ ["required" .= req | not (null req)]

-- | A simple string property.
strProp :: Text -> Value
strProp desc = object ["type" .= ("string" :: Text), "description" .= desc]

-- | A simple boolean property.
boolProp :: Text -> Value
boolProp desc = object ["type" .= ("boolean" :: Text), "description" .= desc]

-- | A simple integer property.
intProp :: Text -> Value
intProp desc = object ["type" .= ("integer" :: Text), "description" .= desc]

-- | An enum string property.
enumProp :: Text -> [Text] -> Value
enumProp desc vals = object
  [ "type" .= ("string" :: Text)
  , "description" .= desc
  , "enum" .= vals
  ]

-- | An array-of-strings property.
arrayProp :: Text -> Value
arrayProp desc = object
  [ "type"  .= ("array" :: Text)
  , "items" .= object ["type" .= ("string" :: Text)]
  , "description" .= desc
  ]

------------------------------------------------------------------------
-- Parameter extraction (arrays)
------------------------------------------------------------------------

getParamArray :: Text -> Value -> [Text]
getParamArray key val = case val of
  Object o -> case KM.lookup (fromText key) o of
    Just (Array arr) -> [t | String t <- toList arr]
    _                -> []
  _ -> []

------------------------------------------------------------------------
-- Argument helpers
------------------------------------------------------------------------

-- | Prefix each element with a tag.
--   @tagEach "--ta" ["-p", "foo"] == ["--ta", "-p", "--ta", "foo"]@
tagEach :: Text -> [Text] -> [Text]
tagEach tag = concatMap (\a -> [tag, a])

------------------------------------------------------------------------
-- Structured error helpers
------------------------------------------------------------------------

-- | Standard structured error for a failed stack command.
mkCommandError :: [Text] -> StackOutput -> ToolResult
mkCommandError args so =
  let diags = parseGhcDiagnostics (soStderr so)
      depErrs = parseDepErrors (soStderr so)
  in mkCommandErrorWithDiags args so diags depErrs Nothing

-- | Standard structured error with pre-parsed diagnostics.
mkCommandErrorWithDiags :: [Text] -> StackOutput -> [GhcDiagnostic] -> [DepError] -> Maybe FilePath -> ToolResult
mkCommandErrorWithDiags args so diags depErrs mcwd = mkToolErrorJSON $ object $
  [ "error_type"   .= ("command_failed" :: Text)
  , "exit_code"    .= soExitCode so
  , "command"      .= T.unwords ("stack" : args)
  , "diagnostics"  .= diagnosticsSummary diags
  , "raw_output"   .= soStdout so
  , "raw_stderr"   .= soStderr so
  ] ++ ["dependency_errors" .= depErrorsSummary depErrs | not (null depErrs)]
    ++ maybe [] (\d -> ["project_root" .= T.pack d]) mcwd

-- | Structured error with optional warning filtering.
--   When @includeWarnings@ is False, warnings are omitted from diagnostics.
--   When @includeOutput@ is False, raw stdout/stderr are omitted.
--   Raw stderr is always included as a fallback when no structured diagnostics were parsed.
mkCommandErrorFiltered :: Bool -> Bool -> [Text] -> StackOutput -> [GhcDiagnostic] -> [DepError] -> Maybe FilePath -> ToolResult
mkCommandErrorFiltered includeWarnings includeOutput args so diags depErrs mcwd =
  let mDiagSummary = filteredDiagnosticsSummary includeWarnings diags
      hasStructured = not (null diags) || not (null depErrs)
  in mkToolErrorJSON $ object $
    [ "error_type"  .= ("command_failed" :: Text)
    , "exit_code"   .= soExitCode so
    , "command"     .= T.unwords ("stack" : args)
    ] ++ maybe [] (\d -> ["diagnostics" .= d]) mDiagSummary
      ++ ["dependency_errors" .= depErrorsSummary depErrs | not (null depErrs)]
      ++ ["raw_output" .= soStdout so | includeOutput]
      ++ ["raw_stderr" .= soStderr so | includeOutput || not hasStructured]
      ++ maybe [] (\d -> ["project_root" .= T.pack d]) mcwd
