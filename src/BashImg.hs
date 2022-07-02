
{-# LANGUAGE OverloadedStrings #-}

module BashImg
  ( setWallpaper
  , createImageFile
  , downloadImageFile ) where

-- Modules
import System.Process (callProcess, readProcess)

import qualified Clippings as C
  ( Quote ( Quote
          , author
          , book
          , quote ) )
import qualified WikimediaAPI as W ( URL )
import qualified Data.Text.Lazy as T ( unpack, fromStrict )

-- Set Wallpaper
setWallpaper :: FilePath -> IO ()
setWallpaper fp = callProcess "plasma-apply-wallpaperimage" [fp]

-- Make image file
createImageFile :: C.Quote -> IO FilePath
createImageFile C.Quote { C.author = a
                        , C.book   = b
                        , C.quote  = q } = do
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
    picDir = "./in.jpg"
    outDir = "./out.jpg"

-- Download image file
downloadImageFile :: W.URL -> IO ()
downloadImageFile url = do
  callProcess "wget"
    [ "--output-document=./in.jpg"
    , (T.unpack . T.fromStrict) url ]