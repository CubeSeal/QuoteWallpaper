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
import Data.Maybe (fromMaybe)

-- | Set Wallpaper
setWallpaper :: MonadIO m => FilePath -> ReaderT FilePath m ()
setWallpaper fp = do
  dir <- ask
  let picDir = dir ++ fp
  liftIO $ callProcess "plasma-apply-wallpaperimage" [picDir]

-- | Make image file
createImageFile 
  :: MonadIO m
  => FilePath
  -> C.AnnotatedQuote
  -> ReaderT FilePath m FilePath
createImageFile inImgFile C.AQuote {..} = do
  dir <- ask
  let
    picDir   = dir ++ inImgFile
    outFile  = "out.jpg"
    outDir   = dir ++ outFile
    quoteStr = T.unpack aQuote
      ++ "\n\n— "
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
    , "EB-Garamond-12-Regular"
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