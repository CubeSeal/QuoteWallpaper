{-# LANGUAGE OverloadedStrings #-}

module BashImg
  ( setWallpaper
  , createImageFile
  , downloadImageFile ) where

-- Modules
import System.Process (callProcess, readProcess)
import UsefulFunctions ( getISODate )
import Control.Monad.Reader (ReaderT, MonadReader (ask), MonadIO (liftIO))
import Data.List ( isInfixOf )

import qualified Clippings as C
  ( Quote ( Quote
          , author
          , book
          , quote ) )
import qualified WikimediaAPI as W ( URL, fetchPOTD )
import qualified Data.Text.Lazy as T ( unpack, fromStrict )
import qualified System.Directory as D (doesFileExist, listDirectory, removeFile)

createImageFile :: C.Quote -> ReaderT FilePath IO FilePath
createImageFile quote = do
  cleanDir
  url <- liftIO W.fetchPOTD
  rawImgFilePath  <- downloadImageFile url
  makeImageFile rawImgFilePath quote

cleanDir :: ReaderT FilePath IO ()
cleanDir = do
  dir <- ask
  listOfFiles <- liftIO $ D.listDirectory dir
  date <- liftIO getISODate
  let p x = isInfixOf "jpg" x && not (isInfixOf date x)
      oldFiles = map (dir ++) . filter p $ listOfFiles
      f x = liftIO $ do
        D.removeFile x
        putStrLn $ "Removed: " ++ x
  mapM_ f oldFiles

-- Set Wallpaper
setWallpaper :: FilePath -> ReaderT FilePath IO ()
setWallpaper fp = do
  dir <- ask
  let picDir = dir ++ fp
  liftIO $ callProcess "plasma-apply-wallpaperimage" [picDir]

-- Make image file
makeImageFile :: FilePath -> C.Quote -> ReaderT FilePath IO FilePath
makeImageFile
  inImgFile
  C.Quote { C.author = a
          , C.book   = b
          , C.quote  = q
          } = do
  formatedQuote <- liftIO $ readProcess "fold" ["-s"] $ T.unpack q
  dir <- ask
  let picDir   = dir ++ inImgFile
      outFile  = "out.jpg"
      outDir   = dir ++ outFile
      printStr = formatedQuote
        <> "\n\n\t— "
        <> T.unpack a
        <> " ("
        <> T.unpack b
        <> ")"
        <> "\n"
  liftIO $ callProcess
    "convert"
    [ "-gravity"
    , "center"
    , "-resize"
    , "1920x1080^"
    , "+sigmoidal-contrast"
    , "3x0%"
    , "-family"
    , "EB Garamond"
    , "-fill"
    , "white"
    , "-pointsize"
    , "50"
    , "-annotate"
    , "+0+0"
    , printStr
    , picDir
    , outDir
    ]
  return outFile

-- Download image file
downloadImageFile :: W.URL -> ReaderT FilePath IO FilePath
downloadImageFile url = do
  date <- liftIO getISODate
  dir  <- ask
  let imgFile = date ++ ".jpg"
      fullPathImgFile = dir ++ imgFile
  doesRawImgFileExist  <- liftIO $ D.doesFileExist fullPathImgFile
  if doesRawImgFileExist
    then
      return imgFile
    else do
      liftIO $ callProcess
        "wget"
        [ "--output-document=" ++ fullPathImgFile
        , (T.unpack . T.fromStrict) url
        ]
      return imgFile