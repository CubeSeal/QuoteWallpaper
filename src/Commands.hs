{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Commands
  ( downloadImageFile
  , cleanDir
  , createImageFile
  , setWallpaper
  ) where

import Control.Monad.Reader
  ( ReaderT
  , MonadReader (ask)
  , MonadIO (liftIO)
  )
import Control.Monad (unless)

import Clippings (AnnotatedQuote (..))
import UsefulFunctions (getISODate)
import Data.List (isInfixOf)
import System.Process ( callProcess ) 

import qualified System.Directory as D
import qualified System.Info as I

import qualified Commands.KDE as KDE
import qualified Commands.Windows as WIN
import qualified WikimediaAPI as W
import qualified Data.Text.Lazy as T

-- Download image file
downloadImageFile :: ReaderT FilePath IO FilePath
downloadImageFile = do
  date <- getISODate
  dir  <- ask
  let
    command = if I.os == "mingw32" then "-OutFile" else "--output-document="
    imgFile = date ++ ".jpg"
    fullPathImgFile = dir ++ imgFile

  doesRawImgFileExist <- liftIO $ D.doesFileExist fullPathImgFile
  unless doesRawImgFileExist $ do
    url <- W.fetchPOTD
    liftIO $ callProcess "wget" [command ++ fullPathImgFile, T.unpack $ T.fromStrict url]

  return imgFile

-- Take downloaded file and add the quote to it.
createImageFile :: FilePath -> AnnotatedQuote -> ReaderT FilePath IO FilePath
createImageFile inFile quote = do
  let formattedQuote = formatQuote quote
  if I.os == "mingw32"
    then WIN.createImageFile inFile formattedQuote
    else KDE.createImageFile inFile formattedQuote

-- Set wallpaper
setWallpaper :: FilePath -> ReaderT FilePath IO ()
setWallpaper q = if I.os == "mingw32"
  then undefined
  else KDE.setWallpaper q

-- Clean 
cleanDir :: ReaderT FilePath IO ()
cleanDir = do
  dir <- ask
  listOfFiles <- liftIO $ D.listDirectory dir
  date <- getISODate
  let p x = isInfixOf "jpg" x
        && not (date `isInfixOf` x)
        && not ("out" `isInfixOf` x)
      oldFiles = map (dir ++) . filter p $ listOfFiles
      f x = liftIO $ do
        D.removeFile x
        putStrLn $ "Removed: " ++ x
  mapM_ f oldFiles

formatQuote :: AnnotatedQuote -> AnnotatedQuote
formatQuote AQuote {..} = AQuote aAuthor aBook formattedQuote formattedNote
  where
    formattedQuote = foldLines 80 aQuote
    formattedNote  = foldLines 80 <$> aNote

-- Truncate Quote so it fits on-screen
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