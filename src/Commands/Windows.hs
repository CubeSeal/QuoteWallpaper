{-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE ForeignFunctionInterface #-}
-- {-# LANGUAGE OverloadedStrings #-}

module Commands.Windows
  ( setWallpaper
  , createImageFile
  ) where

import Control.Monad.Reader ( ReaderT, MonadIO )

import qualified Clippings as C
import App(Env)
-- import Foreign.C (CString)

createImageFile
  :: MonadIO m
  => FilePath
  -> C.AnnotatedQuote
  -> ReaderT Env m FilePath
createImageFile inImgFile C.AQuote {..} = undefined

setWallpaper :: MonadIO m => FilePath -> ReaderT Env m ()
setWallpaper = undefined

-- foreign import ccall "windows.h SetWallpaper" printF :: CString -> IO ()