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
import Data.Maybe (fromMaybe)


createImageFile :: C.AnnotatedQuote -> W.URL -> ReaderT FilePath IO FilePath
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
makeImageFile :: FilePath -> C.AnnotatedQuote -> ReaderT FilePath IO FilePath
makeImageFile inImgFile C.AQuote {..} = do
  dir <- ask
  let
    picDir   = dir ++ inImgFile
    outFile  = "out.jpg"
    outDir   = dir ++ outFile
    quoteStr = T.unpack aQuote
      ++ "\n\n\t— "
      ++ T.unpack aAuthor
      ++ " ("
      ++ T.unpack aBook
      ++ ")"
      ++ "\n"
    noteStr  = T.unpack $ fromMaybe "" aNote
  liftIO $ callProcess
    "convert"
    [ "-gravity"
    , "center"
    , "-resize"
    , "1920x1080^"
    , "-extent"
    , "1920x1080"
    , "+sigmoidal-contrast"
    , "3x0%"
    , "-blur"
    , "0x2"
    , "-font"
    , "EB-Garamond-08-Regular"
    , "-fill"
    , "white"
    , "-pointsize"
    , "50"
    , "-annotate"
    , "+0+0"
    , quoteStr
    , "-font"
    , "EB-Garamond-08-Italic"
    , "-gravity"
    , "southeast"
    , "-annotate"
    , "+100+100"
    , noteStr
    , picDir
    , outDir
    ]
  return outFile