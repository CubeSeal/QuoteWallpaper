-- No clown T.pack and T.unpack
{-# LANGUAGE OverloadedStrings #-}

module Quotewallpaper where

-- Modules
import Control.Monad.Reader (ReaderT(runReaderT), MonadIO, liftIO, filterM)
import Data.Maybe (fromMaybe)
import Data.Foldable (traverse_)
import Data.Char (isPunctuation)

import UsefulFunctions ((!?), getDay)
import App(Env(..), toApiKey)

import qualified Data.Text.Lazy as T
import qualified System.Directory as D
import qualified System.Random as R
import qualified Data.Time.Calendar.OrdinalDate as DT
import qualified Data.Time as DT

import qualified Clippings as C
import qualified Commands as CMD


main :: IO ()
main = do
  -- Substantiate directory.
  dir <- substantiateDir "quotewallpaper/"

  (apiKeyText : clippings : _) <- traverse (readFile . (dir <>))
    [ "Bearer.txt"
    , "My Clippings.txt"
    ]

  let env = Env dir $ toApiKey apiKeyText

  -- Import and parse quotes, and then get random one.
  filteredQuotes <- filterM filterAQuote . C.rawToAQuotes $ T.pack clippings
  ranQuote <- getRanQuote filteredQuotes

  flip runReaderT env $ do
    --CMD.cleanDir Removing so I can keep pictures.
    downloadedFile <- CMD.downloadImageFile ranQuote

    imgFile <- CMD.createImageFile downloadedFile ranQuote
    CMD.setWallpaper imgFile

-- | Create directory if not existing already.
substantiateDir :: MonadIO m => FilePath -> m FilePath
substantiateDir dirname = liftIO $ do
  appDir <- D.getAppUserDataDirectory dirname
  traverse_ (D.createDirectoryIfMissing True . (appDir <>))
    [ "infiles"
    , "outfiles"
    ]
  return appDir

-- | Get Random Quote based on day and year.
getRanQuote :: MonadIO m => [C.AnnotatedQuote] -> m C.AnnotatedQuote
getRanQuote quotes = liftIO $ do
  (yearNum, dayNum) <- DT.toOrdinalDate <$> getDay
  let
    -- Seed that is unique per day.
    dailySeed = fromInteger yearNum + dayNum
    (ranNum, _) = R.uniformR (0, length quotes - 1) $ R.mkStdGen dailySeed
    -- This should be safe.
    ranQuote = fromMaybe undefined $ quotes !? ranNum
  print ranQuote
  return ranQuote

filterAQuote :: C.AnnotatedQuote -> IO Bool
filterAQuote C.AQuote
  { C.aAuthor = a
  , C.aQuote = q
  , C.aDateTime = DT.LocalTime day _
  } = do
  today <- getDay
  let
    p1 = maybe False (isPunctuation . snd) $ T.unsnoc q
    p2 = all (`notElem` T.words a) ["Kenneth", "Fred"]
    p3 = length (T.words q) > 1
    p4 = today `DT.diffDays` day <= 1000
  
  pure $ and
    [ p1
    , p2
    , p3
    , p4
    ]