module Commands
  ( createImageFile
  , setWallpaper
  ) where

import Control.Monad.Reader (ReaderT)

import Clippings (Quote)

import qualified System.Info as I

import qualified Commands.KDE as K

createImageFile :: Quote -> ReaderT FilePath IO FilePath
createImageFile = if I.os == "mingw32" then undefined else K.createImageFile

setWallpaper :: FilePath -> ReaderT FilePath IO ()
setWallpaper q = if I.os == "mingw32" then undefined else K.setWallpaper q