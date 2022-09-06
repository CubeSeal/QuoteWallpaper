{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Commands.KDE
  ( setWallpaper
  , createImageFile
  ) where

-- Modules
import System.Process (callProcess)
import Control.Monad.Reader
  ( ReaderT
  , MonadReader (ask)
  , MonadIO (liftIO)
  )

import qualified Data.Text.Lazy as T

import qualified Clippings as C
import qualified WikimediaAPI as W
import qualified Commands.Common as COM


createImageFile :: C.Quote -> W.URL -> ReaderT FilePath IO FilePath
createImageFile quote url = do
  rawImgFilePath <- COM.downloadImageFile "--output-document=" url
  makeImageFile rawImgFilePath quote

-- Set Wallpaper
setWallpaper :: FilePath -> ReaderT FilePath IO ()
setWallpaper fp = do
  dir <- ask
  let picDir = dir ++ fp
  liftIO $ callProcess "plasma-apply-wallpaperimage" [picDir]

-- Make image file
makeImageFile :: FilePath -> C.Quote -> ReaderT FilePath IO FilePath
makeImageFile inImgFile C.Quote {..} = do
  dir <- ask
  let
    picDir   = dir ++ inImgFile
    outFile  = "out.jpg"
    outDir   = dir ++ outFile
    printStr = T.unpack quote
      ++ "\n\n\t— "
      ++ T.unpack author
      ++ " ("
      ++ T.unpack book
      ++ ")"
      ++ "\n"
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