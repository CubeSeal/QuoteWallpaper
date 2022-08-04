-- No clown T.pack and T.unpack
{-# LANGUAGE OverloadedStrings #-}

module Quotewallpaper where

-- Modules
import Control.Monad.Reader (ReaderT(runReaderT))
import Data.Maybe (fromMaybe)

import UsefulFunctions ((!?))

import qualified Data.Text.Lazy as T
import qualified System.Directory as D

import qualified Clippings as C
import qualified Commands as CMD
import qualified Data.Time.Calendar.OrdinalDate as DT
import qualified Data.Time as CL


main :: IO ()
main = do
  dir    <- substantiateDir "quotewallpaper/"
  quotes <- C.rawToQuotes
    . T.pack
    <$> readFile (dir ++ "My Clippings.txt")
  ranQuote <- getRanQuote quotes
  flip runReaderT dir $ do
    imgFile <- CMD.createImageFile ranQuote
    CMD.setWallpaper imgFile

substantiateDir :: FilePath -> IO FilePath
substantiateDir dirname = do
  appDir <- D.getAppUserDataDirectory dirname
  D.createDirectoryIfMissing True appDir
  return appDir

getRanQuote :: [C.Quote] -> IO C.Quote
getRanQuote quotes = do
  (yearNum, dayNum) <- DT.toOrdinalDate . CL.utctDay <$> CL.getCurrentTime
  let
    ranNum = (fromInteger yearNum + dayNum) `mod` length quotes
    -- This should be safe.
    ranQuote = fromMaybe undefined $ quotes !? ranNum
  return ranQuote