-- No clown T.pack and T.unpack
{-# LANGUAGE OverloadedStrings #-}

module Quotewallpaper where

-- Modules
import qualified Clippings as C ( rawToQuotes )
import qualified Wallpaper  as W ( createImageFile, setWallpaper, downloadImageFile )
import qualified WikimediaAPI as A (fetchPOTD)
import qualified Data.Text.Lazy as T
import qualified Data.Time.Clock as CL ( UTCTime(utctDay), getCurrentTime )
import qualified Data.Time.Calendar.OrdinalDate as DT (toOrdinalDate)

main :: IO ()
main = do
  quotes <- C.rawToQuotes
    .   T.pack
    <$> readFile "./My Clippings.txt"
  (_, dayNum) <- DT.toOrdinalDate
    .   CL.utctDay
    <$> CL.getCurrentTime
  let ranNum = dayNum `mod` length quotes
  -- This should be safe.
  let ranQuote = quotes !! ranNum
  url <- A.fetchPOTD
  W.downloadImageFile url
  imgFile <- W.createImageFile ranQuote
  W.setWallpaper imgFile