{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Commands
  ( downloadImageFile
  , cleanDir
  , createImageFile
  , setWallpaper
  ) where

import Control.Monad.Reader
  ( MonadReader (ask)
  , MonadIO (liftIO), asks
  )
import Control.Monad (unless)

import Clippings (AnnotatedQuote (..))
import UsefulFunctions (getISODate)
import App(Env(..))
import Data.List (isInfixOf)
import System.Process ( callProcess )

import qualified System.Directory as D
import qualified System.Info as I

import qualified Commands.KDE as KDE
import qualified Commands.Windows as WIN
import qualified Data.Text.Lazy as T
import qualified Clippings as C
import qualified DalleDownload as O

-- | Download image file and save to state directory. Returns relative filepath.
downloadImageFile :: (MonadIO m, MonadReader Env m) => C.AnnotatedQuote -> m FilePath
downloadImageFile ranQuote = do
  date <- getISODate
  Env dir apiKey <- ask
  let
    command = if I.os == "mingw32" then "-OutFile" else "--output-document="
    imgFile = date ++ ".jpg"
    fullPathImgFile = dir ++ "infiles/" ++ imgFile

  doesRawImgFileExist <- liftIO $ D.doesFileExist fullPathImgFile

  unless doesRawImgFileExist $ do
    url <- O.fetchDalle3 ranQuote apiKey
    liftIO $ callProcess "wget" [command ++ fullPathImgFile, T.unpack url]

  return imgFile

-- | Take image file and add the quote to it.
createImageFile :: (MonadIO m, MonadReader Env m) => FilePath -> AnnotatedQuote -> m FilePath
createImageFile inFile quote = do
  let formattedQuote = formatQuote quote
  case I.os of
    "mingw32" -> WIN.createImageFile inFile formattedQuote
    "linux" -> KDE.createImageFile inFile formattedQuote
    _ -> undefined --TODO: Add default behaviour.

-- | Set wallpaper.
setWallpaper :: (MonadIO m, MonadReader Env m) => FilePath -> m ()
setWallpaper q = if I.os == "mingw32"
  then undefined
  else KDE.setWallpaper q

-- | Clean directory of extra/junk files.
cleanDir :: (MonadIO m, MonadReader Env m) => m ()
cleanDir = do
  dir <- asks directory
  listOfFiles <- liftIO $ D.listDirectory dir
  date <- getISODate

  let
    p x = isInfixOf "jpg" x && not (date `isInfixOf` x) && not ("out" `isInfixOf` x)
    oldFiles = map (dir ++) . filter p $ listOfFiles
    f x = liftIO $ do
        D.removeFile x
        putStrLn $ "Removed: " ++ x

  mapM_ f oldFiles

-- | Truncate strings so it fits on-screen.
formatQuote :: AnnotatedQuote -> AnnotatedQuote
formatQuote AQuote {..} = AQuote aAuthor aBook formattedQuote formattedNote aDateTime
  where
    formattedQuote = foldLines 80 aQuote
    formattedNote  = foldLines 80 <$> aNote

-- | Cut string into similarly sized lines respecting word boundaries.
foldLines :: Int -> T.Text -> T.Text
foldLines lineLimit str = T.pack $ go (T.unpack str) 0
  where
    -- Main recursive function that goes through string.
    go :: String -> Int -> String
    go [] _ = []
    go (x:xs) i
      | i == lineLimit = x : '\n' : go xs 0
      | x == '\n'      = x : go xs 0
      | x == ' '       = if p xs i then '\n' : go xs 0 else x : go xs (i + 1)
      | otherwise      = x : go xs (i + 1)
    -- Look ahead predicate to see if space is 'last' before 80 line limit is reached.
    p :: String -> Int -> Bool
    p [] _ = False
    p (l:ls) i'
      | i' == lineLimit = True
      | l == ' '        = False
      | otherwise       = p ls (i' + 1)
