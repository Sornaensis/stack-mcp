{-# LANGUAGE OverloadedStrings #-}

module StackMCP.Tools.Parse
  ( -- * GHC Diagnostics
    GhcDiagnostic(..)
  , Severity(..)
  , parseGhcDiagnostics
  , diagnosticsSummary
    -- * Dependency Errors
  , DepError(..)
  , parseDepErrors
  , depErrorsSummary
    -- * Test Failures
  , TestFailure(..)
  , parseTestFailures
  , testFailuresSummary
    -- * Helpers
  , readInt
  ) where

import Data.Aeson
import Data.Text (Text)
import Data.Text qualified as T

------------------------------------------------------------------------
-- GHC Diagnostics
------------------------------------------------------------------------

data Severity = SevError | SevWarning deriving (Show, Eq)

data GhcDiagnostic = GhcDiagnostic
  { diagFile     :: !Text
  , diagLine     :: !Int
  , diagColumn   :: !Int
  , diagSeverity :: !Severity
  , diagCode     :: !(Maybe Text)  -- e.g. "[GHC-88464]" or "[-Wunused-imports]"
  , diagMessage  :: !Text
  } deriving (Show)

instance ToJSON GhcDiagnostic where
  toJSON d = object $
    [ "file"     .= diagFile d
    , "line"     .= diagLine d
    , "column"   .= diagColumn d
    , "severity" .= sevText (diagSeverity d)
    , "message"  .= diagMessage d
    ] ++ maybe [] (\c -> ["code" .= c]) (diagCode d)

sevText :: Severity -> Text
sevText SevError   = "error"
sevText SevWarning = "warning"

-- | Parse GHC diagnostics from stack build stderr output.
--   Extracts structured file\/line\/column\/severity\/message from each diagnostic.
parseGhcDiagnostics :: Text -> [GhcDiagnostic]
parseGhcDiagnostics = go . T.lines
  where
    go [] = []
    go (l:rest) =
      case parseDiagHeader l of
        Just diag ->
          let (body, remaining) = collectBody rest
              msg = case (T.null (diagMessage diag), T.null body) of
                      (True, False)  -> body
                      (False, True)  -> diagMessage diag
                      (False, False) -> diagMessage diag <> "\n" <> body
                      (True, True)   -> diagFile diag <> ":" <> T.pack (show (diagLine diag))
                                         <> ":" <> T.pack (show (diagColumn diag))
          in diag { diagMessage = T.strip msg } : go remaining
        Nothing -> go rest

    -- Collect continuation lines (body of a diagnostic).
    -- Stop at the next diagnostic header or stack progress noise.
    collectBody = go' []
      where
        go' acc [] = (T.unlines (reverse acc), [])
        go' acc (l:rest)
          | isDiagHeader l  = (T.unlines (reverse acc), l:rest)
          | isStackNoise l  = (T.unlines (reverse acc), l:rest)
          | otherwise       = go' (l:acc) rest

    isDiagHeader l = case parseDiagHeader l of
      Just _  -> True
      Nothing -> False

    isStackNoise l =
      any (`T.isPrefixOf` stripped)
        [ "Preprocessing ", "Building ", "Linking ", "Installing "
        , "copy/register" , "Progress "
        ]
      || isCompileProgress stripped
      where stripped = T.stripStart l

    isCompileProgress l =
      T.isPrefixOf "[" l
      && T.isInfixOf " of " l
      && (T.isInfixOf "Compiling " l || T.isInfixOf "] Linking " l)

-- | Try to parse a line as a GHC diagnostic header.
--   Matches: @file:line:col: error: [CODE] message@ and warning variant.
parseDiagHeader :: Text -> Maybe GhcDiagnostic
parseDiagHeader line =
  case tryWith ": error:" SevError line of
    Just d  -> Just d
    Nothing -> tryWith ": warning:" SevWarning line
  where
    tryWith marker sev l =
      case T.breakOn marker l of
        (_, "")     -> Nothing  -- marker not found
        (before, after) ->
          let afterMarker = T.strip (T.drop (T.length marker) after)
              (mCode, msg) = extractCode afterMarker
          in case parseFileLineCol before of
               Just (file, ln, col) -> Just $ GhcDiagnostic file ln col sev mCode msg
               Nothing -> Nothing

    extractCode t
      | "[" `T.isPrefixOf` t =
          case T.breakOn "]" t of
            (_, "") -> (Nothing, t)
            (code, rest) -> (Just (code <> "]"), T.strip (T.drop 1 rest))
      | otherwise = (Nothing, t)

    -- Parse "filepath:line:col" from the end.
    -- Handles Windows drive-letter paths like "C:\foo\bar.hs:10:5".
    parseFileLineCol t =
      let parts = T.splitOn ":" t
      in case reverse parts of
           (colT:lineT:fileParts) ->
             case (readInt (T.strip colT), readInt (T.strip lineT)) of
               (Just col, Just ln) ->
                 let file = T.intercalate ":" (reverse fileParts)
                 in if T.null file then Nothing else Just (file, ln, col)
               _ -> Nothing
           _ -> Nothing

-- | Produce a JSON summary of parsed diagnostics.
diagnosticsSummary :: [GhcDiagnostic] -> Value
diagnosticsSummary diags =
  let errs  = filter ((== SevError)   . diagSeverity) diags
      warns = filter ((== SevWarning) . diagSeverity) diags
  in object
    [ "errors"        .= errs
    , "warnings"      .= warns
    , "error_count"   .= length errs
    , "warning_count" .= length warns
    ]

------------------------------------------------------------------------
-- Dependency Errors
------------------------------------------------------------------------

-- | A structured dependency resolution error from Stack.
data DepError = DepError
  { depPackage :: !Text      -- ^ e.g. "aeson-2.1.0.0"
  , depMessage :: !Text      -- ^ full error message for this entry
  } deriving (Show)

instance ToJSON DepError where
  toJSON d = object
    [ "package" .= depPackage d
    , "message" .= depMessage d
    ]

-- | Parse dependency resolution errors from Stack stderr.
--   Handles patterns like:
--
--   * @In the dependencies for foo: aeson needed, but the stack configuration has no specified version@
--   * @- aeson-2.1.0.0 from stack configuration does not match ...@
--   * @Error: While constructing the build plan, the following exceptions were encountered:@
parseDepErrors :: Text -> [DepError]
parseDepErrors = go . T.lines
  where
    go [] = []
    go (l:rest)
      -- "In the dependencies for ...: <pkg> needed, but ..."
      | "In the dependencies for " `T.isInfixOf` l
      , Just pkg <- extractNeeded l =
          DepError pkg (T.strip l) : go rest
      -- "- <pkg>-<ver> from stack configuration does not match ..."
      | Just stripped <- T.stripPrefix "- " (T.strip l)
      , " from " `T.isInfixOf` stripped || " not found" `T.isInfixOf` stripped =
          let pkg = T.takeWhile (/= ' ') stripped
          in DepError pkg (T.strip l) : go rest
      -- "<pkg> must match ..." (cabal solver style)
      | " must match " `T.isInfixOf` l =
          let pkg = T.takeWhile (/= ' ') (T.strip l)
          in DepError pkg (T.strip l) : go rest
      | otherwise = go rest

    extractNeeded l =
      case T.breakOn ": " (snd (T.breakOn "In the dependencies for " l)) of
        (_, afterColon)
          | not (T.null afterColon) ->
              let rest' = T.drop 2 afterColon
                  pkg = T.takeWhile (\c -> c /= ' ' && c /= ',') rest'
              in if T.null pkg then Nothing else Just pkg
          | otherwise -> Nothing

-- | Produce a JSON summary of parsed dependency errors.
depErrorsSummary :: [DepError] -> Value
depErrorsSummary deps = object
  [ "dependency_error_count" .= length deps
  , "dependency_errors"      .= deps
  ]

------------------------------------------------------------------------
-- Test Failures
------------------------------------------------------------------------

data TestFailure = TestFailure
  { tfName     :: !Text
  , tfDetails  :: !Text
  , tfLocation :: !(Maybe Text)
  } deriving (Show)

instance ToJSON TestFailure where
  toJSON f = object $
    [ "name"    .= tfName f
    , "details" .= tfDetails f
    ] ++ maybe [] (\l -> ["location" .= l]) (tfLocation f)

-- | Parse test failures from combined stdout\/stderr.
--   Handles hspec and tasty output formats.
parseTestFailures :: Text -> [TestFailure]
parseTestFailures output =
  let hspec = parseHspecFailures (T.lines output)
      tasty = parseTastyFailures (T.lines output)
  in if not (null hspec) then hspec else tasty

-- | Produce a JSON summary of test failures.
testFailuresSummary :: [TestFailure] -> Value
testFailuresSummary failures = object
  [ "failure_count" .= length failures
  , "failures"      .= failures
  ]

------------------------------------------------------------------------
-- Hspec failure parsing
------------------------------------------------------------------------

-- | Parse hspec-style failures.
--   Format:
--
-- @
-- Failures:
--
--   src\/Test.hs:15:3:
--   1) Module.function should do thing
--        expected: True
--         but got: False
-- @
parseHspecFailures :: [Text] -> [TestFailure]
parseHspecFailures = goHspec Nothing
  where
    goHspec _ [] = []
    goHspec prevLine (l:rest) =
      case parseHspecHeader l of
        Just name ->
          let loc = prevLine >>= extractFileLoc
              (details, remaining) = collectDetails rest
              detailText = T.strip (T.unlines details)
          in TestFailure name detailText loc : goHspec Nothing remaining
        Nothing -> goHspec (Just l) rest

    parseHspecHeader l =
      let stripped = T.stripStart l
      in case T.span isDigitChar stripped of
           (digits, rest)
             | not (T.null digits)
             , Just rest' <- T.stripPrefix ") " rest ->
                 Just (T.strip rest')
           _ -> Nothing

    -- Check if a line looks like a source location: "  src/Foo.hs:10:3:"
    extractFileLoc l =
      let stripped = T.strip l
      in if (T.isInfixOf ":" stripped)
            && (T.isInfixOf ".hs" stripped || T.isInfixOf ".lhs" stripped)
         then Just (T.dropWhileEnd (== ':') stripped)
         else Nothing

    -- Collect deeply-indented detail lines until next failure header or unindented line
    collectDetails = go' []
      where
        go' acc [] = (reverse acc, [])
        go' acc (l:rest)
          | T.null (T.strip l) = go' acc rest  -- skip blank lines
          | isIndented l       = go' (T.strip l : acc) rest
          | otherwise          = (reverse acc, l:rest)
        isIndented l = indentLevel l >= (4 :: Int)
        indentLevel :: Text -> Int
        indentLevel = go'' 0
          where
            go'' :: Int -> Text -> Int
            go'' n t = case T.uncons t of
              Just (' ', rest')  -> go'' (n + 1) rest'
              Just ('\t', rest') -> go'' (n + 8) rest'
              _                  -> n

------------------------------------------------------------------------
-- Tasty failure parsing
------------------------------------------------------------------------

-- | Parse tasty-style failures.
--   Format:
--
-- @
--   Tests
--     unit tests
--       addition:       OK
--       subtraction:    FAIL
--         expected: 5
--          but got: 3
-- @
parseTastyFailures :: [Text] -> [TestFailure]
parseTastyFailures = goTasty
  where
    goTasty [] = []
    goTasty (l:rest)
      | isTastyFail l =
          let name = extractTastyName l
              baseIndent = indentLevel l
              (details, remaining) = collectTastyDetails baseIndent rest
              detailText = T.strip (T.unlines (map T.strip details))
          in TestFailure name detailText Nothing : goTasty remaining
      | otherwise = goTasty rest

    isTastyFail l =
      let stripped = T.stripEnd l
      in case T.stripSuffix "FAIL" stripped of
           Just prefix ->
             let p = T.stripEnd prefix
             in not (T.null p) && T.last p == ':'
           Nothing -> False

    extractTastyName l =
      let stripped = T.strip l
      in case T.stripSuffix "FAIL" stripped of
           Just prefix -> T.strip (T.dropWhileEnd (\c -> c == ':' || c == ' ') prefix)
           Nothing     -> stripped

    collectTastyDetails baseIndent = go' []
      where
        go' acc [] = (reverse acc, [])
        go' acc (l:rest)
          | T.null (T.strip l) = go' acc rest
          | indentLevel l > baseIndent = go' (l:acc) rest
          | otherwise = (reverse acc, l:rest)

    -- Count indent level, treating tabs as 8 spaces
    indentLevel :: Text -> Int
    indentLevel = go' 0
      where
        go' :: Int -> Text -> Int
        go' n t = case T.uncons t of
          Just (' ', rest)  -> go' (n + 1) rest
          Just ('\t', rest) -> go' (n + 8) rest
          _                 -> n

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

readInt :: Text -> Maybe Int
readInt t = case reads (T.unpack t) of
  [(n, "")] -> Just n
  _         -> Nothing

isDigitChar :: Char -> Bool
isDigitChar c = c >= '0' && c <= '9'
