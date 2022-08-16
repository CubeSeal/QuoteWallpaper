{-# LANGUAGE OverloadedStrings #-}

module Commands
  ( createImageFile
  , setWallpaper
  ) where

import Control.Monad.Reader
  ( ReaderT
  , MonadReader (ask)
  , MonadIO (liftIO)
  )

import Clippings (Quote)
import UsefulFunctions (getISODate)
import Data.List (isInfixOf)

import qualified System.Directory as D
import qualified System.Info as I

import qualified Commands.KDE as KDE
import qualified Commands.Windows as WIN
import qualified WikimediaAPI as W

createImageFile :: Quote -> ReaderT FilePath IO FilePath
createImageFile quote = do
  cleanDir
  url <- liftIO W.fetchPOTD
  if I.os == "mingw32"
    then WIN.createImageFile quote url
    else KDE.createImageFile quote url

cleanDir :: ReaderT FilePath IO ()
cleanDir = do
  dir <- ask
  listOfFiles <- liftIO $ D.listDirectory dir
  date <- liftIO getISODate
  let p x = isInfixOf "jpg" x
        && not (date `isInfixOf` x)
        && not ("out" `isInfixOf` x)
      oldFiles = map (dir ++) . filter p $ listOfFiles
      f x = liftIO $ do
        D.removeFile x
        putStrLn $ "Removed: " ++ x
  mapM_ f oldFiles

setWallpaper :: FilePath -> ReaderT FilePath IO ()
setWallpaper q = if I.os == "mingw32"
  then undefined
  else KDE.setWallpaper q
