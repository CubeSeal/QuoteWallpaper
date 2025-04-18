{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE ForeignFunctionInterface #-}
-- {-# LANGUAGE OverloadedStrings #-}

module Commands.Windows
  ( setWallpaper
  , createImageFile
  ) where

import Control.Monad.Reader ( MonadIO, MonadReader)

import qualified Clippings as C
import App(Env)
-- import Foreign.C (CString)

createImageFile
  :: (MonadIO m, MonadReader Env m)
  => FilePath
  -> C.AnnotatedQuote
  -> m FilePath
createImageFile inImgFile C.AQuote {..} = undefined

setWallpaper :: (MonadIO m, MonadReader Env m) => FilePath -> m ()
setWallpaper = undefined

-- foreign import ccall "windows.h SetWallpaper" printF :: CString -> IO ()
