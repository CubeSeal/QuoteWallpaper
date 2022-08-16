module Commands.Common
 (downloadImageFile) where

import Control.Monad.Reader
  ( ReaderT
  , MonadReader (ask)
  , MonadIO (liftIO)
  )
import System.Process (callProcess)

import UsefulFunctions (getISODate)

import qualified System.Directory as D
import qualified Data.Text.Lazy as T

import qualified WikimediaAPI as W


-- Download image file
downloadImageFile :: String -> W.URL -> ReaderT FilePath IO FilePath
downloadImageFile command url = do
  date <- liftIO getISODate
  dir  <- ask
  let
    imgFile = date ++ ".jpg"
    fullPathImgFile = dir ++ imgFile
  doesRawImgFileExist <- liftIO $ D.doesFileExist fullPathImgFile
  if doesRawImgFileExist
    then
      return imgFile
    else do
      liftIO $ callProcess
        "wget"
        [ command ++ fullPathImgFile
        , (T.unpack . T.fromStrict) url
        ]
      return imgFile