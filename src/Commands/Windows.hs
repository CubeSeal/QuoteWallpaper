{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module Commands.Windows
  ( setWallpaper
  , createImageFile
  ) where

import Control.Monad.Reader ( ReaderT )

import qualified Clippings as C
import Foreign.C (CString)

createImageFile :: FilePath -> C.AnnotatedQuote -> ReaderT FilePath IO FilePath
createImageFile inImgFile C.AQuote {..} = undefined

setWallpaper :: FilePath -> ReaderT FilePath IO ()
setWallpaper = undefined

foreign import ccall "windows.h SetWallpaper" printF :: CString -> IO ()