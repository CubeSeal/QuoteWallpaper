-- No clown T.pack and T.unpack
{-# LANGUAGE OverloadedStrings #-}

module Quotewallpaper where

-- Modules
import Control.Monad.Reader (ReaderT(runReaderT))

import qualified Clippings as C ( rawToQuotes, Quote )
import qualified BashImg as B
  ( createImageFile
  , setWallpaper
  )
import qualified Data.Text.Lazy as T
import qualified System.Random as R (uniformR, initStdGen)
import qualified System.Directory as D (getAppUserDataDirectory, createDirectoryIfMissing)

main :: IO ()
main = do
  dir    <- substantiateDir "quotewallpaper/"
  quotes <- C.rawToQuotes
    . T.pack
    <$> readFile (dir ++ "My Clippings.txt")
  ranQuote <- getRanQuote quotes
  flip runReaderT dir $ do
    imgFile <- B.createImageFile ranQuote
    B.setWallpaper imgFile

substantiateDir :: FilePath -> IO FilePath
substantiateDir dirname = do
  appDir <- D.getAppUserDataDirectory dirname
  D.createDirectoryIfMissing True appDir
  return appDir

getRanQuote :: [C.Quote] -> IO C.Quote
getRanQuote quotes = do
  stdGen <- R.initStdGen
  let (ranNum, _) = R.uniformR (0, length quotes - 1) stdGen
  -- This should be safe.
  return $ quotes !! ranNum