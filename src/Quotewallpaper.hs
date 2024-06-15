-- No clown T.pack and T.unpack
{-# LANGUAGE OverloadedStrings #-}

module Quotewallpaper where

-- Modules
-- Modules
-- Modules
-- Modules
-- Modules
-- Modules
-- Modules
-- Modules
import Control.Monad.Reader (ReaderT(runReaderT), MonadIO)
import Data.Maybe (fromMaybe)

import UsefulFunctions ((!?), withReadFile, Env(..), toApiKey)

import qualified Data.Text.Lazy as T
import qualified System.Directory as D
import qualified System.Random as R

import qualified Clippings as C
import qualified Commands as CMD
import qualified Data.Time.Calendar.OrdinalDate as DT
import qualified Data.Time as CL


main :: IO ()
main = do
  -- Substantiate directory.
  dir <- substantiateDir "quotewallpaper/"
  
  withReadFile (dir <> "Bearer.txt") $ \apiKeyText ->
    withReadFile (dir ++ "My Clippings.txt") $ \clippings -> do
      -- Import and parse quotes, and then get random one.
      ranQuote <- getRanQuote . C.rawToAQuotes $ T.pack clippings
      flip runReaderT (Env dir (toApiKey apiKeyText)) $ setQuoteWallpaper ranQuote


-- | Download POTD file, add ran quote and set wallpaper.
setQuoteWallpaper :: MonadIO m => C.AnnotatedQuote -> ReaderT Env m ()
setQuoteWallpaper ranQuote = do
  --CMD.cleanDir Removing so I can keep pictures.
  downloadedFile <- CMD.downloadImageFile ranQuote

  imgFile <- CMD.createImageFile downloadedFile ranQuote
  CMD.setWallpaper imgFile

-- | Create directory if not existing already.
substantiateDir :: FilePath -> IO FilePath
substantiateDir dirname = do
  appDir <- D.getAppUserDataDirectory dirname
  D.createDirectoryIfMissing True $ appDir ++ "infiles"
  D.createDirectoryIfMissing True $ appDir ++ "outfiles"
  return appDir

-- | Get Random Quote based on day and year.
getRanQuote :: [C.AnnotatedQuote] -> IO C.AnnotatedQuote
getRanQuote quotes = do
  (yearNum, dayNum) <- DT.toOrdinalDate . CL.utctDay <$> CL.getCurrentTime
  let
    -- Seed that is unique per day.
    dailySeed = fromInteger yearNum + dayNum
    (ranNum, _) = R.uniformR (0, length quotes - 1) $ R.mkStdGen dailySeed
    -- This should be safe.
    ranQuote = fromMaybe undefined $ quotes !? ranNum
  return ranQuote