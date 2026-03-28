{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StackMCP.Resources
  ( listResources
  , readResource
  ) where

import Control.Exception (SomeException, catch, try)
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (doesFileExist, listDirectory, getCurrentDirectory, canonicalizePath)
import System.FilePath ((</>), takeExtension, normalise, isRelative)
import System.Info (os)
import StackMCP.Types

-- | List available resources based on the current project directory.
listResources :: IORef (Maybe FilePath) -> IO [ResourceDef]
listResources cwdRef = do
  dir <- getDir cwdRef
  stackYaml   <- doesFileExist (dir </> "stack.yaml")
  packageYaml <- doesFileExist (dir </> "package.yaml")
  lockFile    <- doesFileExist (dir </> "stack.yaml.lock")
  files       <- listDirSafe dir
  let cabalFiles = filter (\f -> takeExtension f == ".cabal") files
  pure $ concat
    [ [ mkRes "stack.yaml" "Stack project configuration" "application/x-yaml"
      | stackYaml ]
    , [ mkRes "package.yaml" "Hpack package configuration" "application/x-yaml"
      | packageYaml ]
    , [ mkRes (T.pack f) ("Cabal package description: " <> T.pack f) "text/plain"
      | f <- cabalFiles ]
    , [ mkRes "stack.yaml.lock" "Stack lock file" "application/x-yaml"
      | lockFile ]
    ]

-- | Read a resource by URI.
readResource :: IORef (Maybe FilePath) -> Text -> IO (Either Text ResourceContent)
readResource cwdRef uri
  | Just suffix <- T.stripPrefix "stack://project/" uri = do
      dir <- getDir cwdRef
      let filename = T.unpack suffix
      -- Reject path traversal attempts
      if not (isRelative filename) || ".." `elem` splitPath' filename
        then pure $ Left "Invalid resource path"
        else do
          let raw = dir </> normalise filename
          canonical <- try (canonicalizePath raw) :: IO (Either SomeException FilePath)
          canonDir  <- canonicalizePath dir
          case canonical of
            Left _  -> pure $ Left ("Resource not found: " <> uri)
            Right resolved
              | not (canonDir `isPrefixOfPath` resolved) ->
                  pure $ Left "Invalid resource path"
              | otherwise -> do
                  exists <- doesFileExist resolved
                  if exists
                    then do
                      result <- try (TIO.readFile resolved) :: IO (Either SomeException Text)
                      case result of
                        Right content -> pure $ Right ResourceContent
                          { rcUri = uri, rcMimeType = Just (guessMime filename), rcText = content }
                        Left err -> pure $ Left ("Error reading file: " <> T.pack (show err))
                    else pure $ Left ("Resource not found: " <> uri)
  | otherwise = pure $ Left ("Unknown resource URI scheme: " <> uri)

-- | Check if one path is a prefix of another (for directory containment).
--   Uses case-insensitive comparison only on Windows.
isPrefixOfPath :: FilePath -> FilePath -> Bool
isPrefixOfPath dir file =
  let d = addTrailingSep (normalise dir)
  in d `isPrefixOf'` normalise file || normalise dir == normalise file
  where
    addTrailingSep p
      | null p         = p
      | last p == '/'  = p
      | last p == '\\' = p
      | otherwise      = p ++ "/"
    isPrefixOf' [] _          = True
    isPrefixOf' _ []          = False
    isPrefixOf' (x:xs) (y:ys)
      | normC x == normC y     = isPrefixOf' xs ys
      | isSep x && isSep y     = isPrefixOf' xs ys
      | otherwise               = False
    isSep c = c == '/' || c == '\\'
    -- Case-fold only on Windows
    normC c
      | isWindows, c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise = c
    isWindows = os == "mingw32"

-- | Split a file path into components for traversal checking.
splitPath' :: FilePath -> [String]
splitPath' = go []
  where
    go acc [] = [reverse acc | not (null acc)]
    go acc (c:cs)
      | c == '/' || c == '\\' = (if null acc then go [] cs else reverse acc : go [] cs)
      | otherwise             = go (c:acc) cs

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

mkRes :: Text -> Text -> Text -> ResourceDef
mkRes name desc mime = ResourceDef
  { resourceUri         = "stack://project/" <> name
  , resourceName        = name
  , resourceDescription = Just desc
  , resourceMimeType    = Just mime
  }

getDir :: IORef (Maybe FilePath) -> IO FilePath
getDir cwdRef = do
  mcwd <- readIORef cwdRef
  case mcwd of
    Just d  -> pure d
    Nothing -> getCurrentDirectory

listDirSafe :: FilePath -> IO [FilePath]
listDirSafe dir = listDirectory dir `catch` (\(_ :: SomeException) -> pure [])

guessMime :: FilePath -> Text
guessMime path
  | ext == ".yaml" || ext == ".yml" = "application/x-yaml"
  | ext == ".cabal" = "text/plain"
  | ext == ".lock"  = "application/x-yaml"
  | otherwise       = "text/plain"
  where ext = takeExtension path
