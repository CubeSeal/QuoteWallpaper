
{-# LANGUAGE OverloadedStrings #-}

module Wallpaper where

-- Modules
import Clippings ( Quote(Quote, author, book, quote) )
import Data.Text.Lazy as T ( unpack )
import System.Process (callProcess, readProcess)

-- Set Wallpaper
setWallpaper :: FilePath -> IO ()
setWallpaper fp = callProcess "plasma-apply-wallpaperimage" [fp]

-- Make image file
createImageFile :: Quote -> IO FilePath
createImageFile Quote { author = a
                      , book   = b
                      , quote  = q } = do
  let printTxt = q <> " - " <> a <> " (" <> b <> ")" <> "\n"
  formatQuote <- readProcess "fold" ["-s"] $ T.unpack printTxt
  callProcess "convert"
    [ "-gravity"
    , "center"
    , "-resize"
    , "1920x1080^"
    , "-blur"
    , "0x5"
    , "-weight"
    , "10"
    , "-family"
    , "EB Garamond"
    , "-fill"
    , "white"
    , "-pointsize"
    , "50"
    , "-annotate"
    , "+0+0"
    , formatQuote
    , picDir
    , outDir ]
  return outDir
  where
    picDir = "/home/landseal/.cache/plasma_engine_potd/wcpotd"
    outDir = "./test.jpeg"