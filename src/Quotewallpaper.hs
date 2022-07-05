-- No clown T.pack and T.unpack
{-# LANGUAGE OverloadedStrings #-}

module Quotewallpaper where

-- Modules
import qualified Clippings as C ( rawToQuotes )
import qualified BashImg as B
  ( createImageFile
  , setWallpaper
  , downloadImageFile )
import qualified WikimediaAPI as W (fetchPOTD)
import qualified Data.Text.Lazy as T
import qualified System.Random as R (uniformR, initStdGen)

main :: IO ()
main = do
  quotes <- C.rawToQuotes . T.pack <$> readFile "./My Clippings.txt"
  stdGen <- R.initStdGen
  let (ranNum, _) = R.uniformR (0, length quotes - 1) stdGen
  -- This should be safe.
  let ranQuote = quotes !! ranNum
  url <- W.fetchPOTD
  B.downloadImageFile url
  imgFile <- B.createImageFile ranQuote
  B.setWallpaper imgFile