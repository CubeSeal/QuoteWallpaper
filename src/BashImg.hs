{-# LANGUAGE OverloadedStrings #-}

module BashImg
  ( setWallpaper
  , createImageFile
  , downloadImageFile ) where

-- Modules
import System.Process (callProcess, readProcess)
import UsefulFunctions ( getISODate )

import qualified Clippings as C
  ( Quote ( Quote
          , author
          , book
          , quote ) )
import qualified WikimediaAPI as W ( URL )
import qualified Data.Text.Lazy as T ( unpack, fromStrict )
import qualified System.Directory as D (doesFileExist)

-- Set Wallpaper
setWallpaper :: FilePath -> IO ()
setWallpaper fp = callProcess "plasma-apply-wallpaperimage" [fp]

-- Make image file
createImageFile :: C.Quote -> IO FilePath
createImageFile C.Quote { C.author = a
                        , C.book   = b
                        , C.quote  = q } = do
  formatedQuote <- readProcess "fold" ["-s"] $ T.unpack q
  date          <- getISODate
  let picDir   = "./" ++ date ++ ".jpg"
      printStr = formatedQuote
               <> "\n\n\t— "
               <> T.unpack a
               <> " ("
               <> T.unpack b
               <> ")"
               <> "\n"
  callProcess "convert"
    [ "-gravity"
    , "center"
    , "-resize"
    , "1920x1080^"
    , "+sigmoidal-contrast"
    , "3x0%"
    , "-family"
    , "EB Garamond"
    , "-fill"
    , "white"
    , "-pointsize"
    , "50"
    , "-annotate"
    , "+0+0"
    , printStr
    , picDir
    , outDir ]
  return outDir
  where
    outDir = "/home/landseal/Documents/QuoteWallpaper/out.jpg"

-- Download image file
downloadImageFile :: W.URL -> IO ()
downloadImageFile url = do
  date <- getISODate
  ind  <- D.doesFileExist $ date ++ ".jpg"
  if ind
    then
      return ()
    else
      callProcess "wget"
        [ "--output-document=/home/landseal/Documents/QuoteWallpaper/" ++ date ++ ".jpg"
        , (T.unpack . T.fromStrict) url ]