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
import Data.Maybe (fromMaybe)

import qualified Data.Text.Lazy as T

import UsefulFunctions (getISODate, Env (..))

import qualified Clippings as C

-- | Set Wallpaper
setWallpaper :: MonadIO m => FilePath -> ReaderT Env m ()
setWallpaper fp = do
  Env dir _ <- ask
  let picDir = dir ++ "outfiles/" ++ fp
  liftIO $ callProcess "plasma-apply-wallpaperimage" [picDir]

-- | Make image file
createImageFile 
  :: MonadIO m
  => FilePath
  -> C.AnnotatedQuote
  -> ReaderT Env m FilePath
createImageFile inImgFile C.AQuote {..} = do
  Env dir _ <- ask
  date <- getISODate
  let
    picDir   = dir ++ "infiles/" ++ inImgFile
    outFile  = "out-" ++ date ++ ".jpg"
    outDir   = dir ++ "outfiles/" ++ outFile
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
    , "0x0"
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