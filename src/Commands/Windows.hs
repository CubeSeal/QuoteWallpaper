{-# LANGUAGE RecordWildCards #-}

module Commands.Windows
  ( setWallpaper
  , createImageFile
  ) where

import Control.Monad.Reader ( ReaderT )

import qualified Clippings as C
import qualified Commands.Common as COM
import qualified WikimediaAPI as W

createImageFile :: C.AnnotatedQuote -> W.URL -> ReaderT FilePath IO FilePath
createImageFile quote url = do
  rawImgFilePath <- COM.downloadImageFile "-OutFile " url
  makeImageFile rawImgFilePath quote

makeImageFile :: FilePath -> C.AnnotatedQuote -> ReaderT FilePath IO FilePath
makeImageFile inImgFile C.AQuote {..} = undefined

setWallpaper :: FilePath -> ReaderT FilePath IO ()
setWallpaper = undefined