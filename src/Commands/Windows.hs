{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module Commands.Windows
  ( setWallpaper
  , createImageFile
  ) where

import Control.Monad.Reader ( ReaderT )

import qualified Clippings as C
import qualified Commands.Common as COM
import qualified WikimediaAPI as W
import Foreign.C (CString)

createImageFile :: C.AnnotatedQuote -> W.URL -> ReaderT FilePath IO FilePath
createImageFile quote url = do
  rawImgFilePath <- COM.downloadImageFile "-OutFile " url
  makeImageFile rawImgFilePath quote

makeImageFile :: FilePath -> C.AnnotatedQuote -> ReaderT FilePath IO FilePath
makeImageFile inImgFile C.AQuote {..} = undefined

setWallpaper :: FilePath -> ReaderT FilePath IO ()
setWallpaper = undefined

foreign import ccall "windows.h SetWallpaper" printF :: CString -> IO ()