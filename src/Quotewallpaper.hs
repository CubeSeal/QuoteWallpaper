-- No clown T.pack and T.unpack
{-# LANGUAGE OverloadedStrings #-}

module Quotewallpaper where

-- Modules
import Control.Monad.Reader (ReaderT(runReaderT))
import Data.Maybe (fromMaybe)
import System.Exit (exitSuccess)

import UsefulFunctions ((!?))

import qualified Data.Text.Lazy as T
import qualified System.Directory as D
import qualified System.Random as R

import qualified Clippings as C
import qualified Commands as CMD
import qualified Data.Time.Calendar.OrdinalDate as DT
import qualified Data.Time as CL


main :: IO ()
main = do
  -- Substantiate directory and check for kindle quotes file.
  dir    <- substantiateDir "quotewallpaper/"
  checkMyClippingsExists dir
  
  -- Import and parse qutoes, and then get random one.
  quotes <- C.rawToAQuotes
    . T.pack
    <$> readFile (dir ++ "My Clippings.txt")
  ranQuote <- getRanQuote quotes

  -- Download POTD file, add ran quote and set wallpaper.
  flip runReaderT dir $ do
    imgFile <- CMD.createImageFile ranQuote
    CMD.setWallpaper imgFile

-- Create directory if not existing already.
substantiateDir :: FilePath -> IO FilePath
substantiateDir dirname = do
  appDir <- D.getAppUserDataDirectory dirname
  D.createDirectoryIfMissing True appDir
  return appDir

-- Check for "My Clippings.txt" file and exit if not there.
checkMyClippingsExists :: FilePath -> IO ()
checkMyClippingsExists dir = do
  p <- D.doesFileExist $ dir ++ "My Clippings.txt"
  if p
    then return ()
    else do
      putStrLn $ "Add My Clipping.txt to " ++ dir
      exitSuccess

-- Get Random Quote based on day and year.
getRanQuote :: [C.AnnotatedQuote] -> IO C.AnnotatedQuote
getRanQuote quotes = do
  (yearNum, dayNum) <- DT.toOrdinalDate . CL.utctDay <$> CL.getCurrentTime
  let
    -- Seed that is unique per day.
    lenQuote = length quotes
    dailySeed = (fromInteger yearNum + dayNum) `mod` lenQuote
    (ranNum, _) = R.uniformR (0, lenQuote - 1) $ R.mkStdGen dailySeed
    -- This should be safe.
    ranQuote = fromMaybe undefined $ quotes !? ranNum
  return ranQuote