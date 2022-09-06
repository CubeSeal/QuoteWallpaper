module Commands.Common
 ( downloadImageFile
 , foldLines
 ) where

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

-- Truncate Quote so it fits on-screen
foldLines :: Int -> String -> String
foldLines lineLimit str = go str 0
  where
    -- Main recursive function that goes through string.
    go :: String -> Int -> String
    go [] _ = []
    go (x:xs) i
      | i == lineLimit = x : '\n' : go xs 0
      | x == ' ' = if p xs i
        then '\n' : go xs 0
        else x : go xs (i + 1)
      | x == '\n' = x : go xs 0
      | otherwise = x : go xs (i + 1)
    -- Look ahead predicate to see if space is 'last' before 80 line limit is reached.
    p :: String -> Int -> Bool
    p [] _ = False
    p (l:ls) i'
      | i' == lineLimit = True
      | l == ' ' = False
      | otherwise = p ls (i' + 1)
