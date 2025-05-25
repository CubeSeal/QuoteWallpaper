{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Commands.KDE
  ( setWallpaper
  , createImageFile
  ) where

-- Modules
import System.Process (callProcess, callCommand)
import Control.Monad.Reader
  ( MonadReader (ask)
  , MonadIO (liftIO)
  )
import Data.List (foldl')

import qualified Data.Text.Lazy as T

import UsefulFunctions (getISODate)
import App(Env(..))

import qualified Clippings as C

-- | Set Wallpaper
setWallpaper :: (MonadIO m, MonadReader Env m) => FilePath -> m ()
setWallpaper fp = do
  Env dir _ <- ask
  let picDir = dir ++ "outfiles/" ++ fp
  liftIO $ callProcess "plasma-apply-wallpaperimage" [picDir]

-- | Escape quotes
quoteEscape :: Bool -> T.Text -> T.Text
quoteEscape handleSingleQuote txt = foldl' (\t (o, n) -> T.replace o n t) txt replaceList2
  where
    replaceList =
      [ ("\\", "\\\\")
      , ("\"", "\\\"")
      , ("$", "\\$")
      , ("`", "\\`")
      , ("$", "\\$")
      ]
    replaceList2 = replaceList <>
      if handleSingleQuote
        then [("'", "'\\\''")]
        else [("'", "\\\'")]

-- | Make image file
createImageFile
  :: (MonadIO m, MonadReader Env m)
  => FilePath
  -> C.AnnotatedQuote
  -> m FilePath
createImageFile inImgFile C.AQuote {..} = do
  Env dir _ <- ask
  date <- getISODate
  let
    infixl 5 <|>
    (<|>) x y = x <> " " <> y
    font_size = 35 :: Integer
    picDir   = dir ++ "infiles/" ++ inImgFile
    outFile  = "out-" ++ date ++ ".jpg"
    outDir   = dir ++ "outfiles/" ++ outFile
    quoteStr =
      aQuote
      <> "\n\n— "
      <> aAuthor
      <> " ("
      <> aBook
      <> ")"
      <> "\n"
    noteStr = T.unpack $
      case aNote of
        Nothing -> ""
        Just text -> "-gravity southeast -annotate +100+100" <|> ("\"" <> text <> "\"")
  liftIO $ callCommand $
    "convert"
    <|> picDir
    <|> "-gravity"
    <|> "center"
    <|> "-resize"
    <|> "1920x1080^"
    <|> "-extent"
    <|> "1920x1080"
    <|> "+sigmoidal-contrast"
    <|> "3x0%"

    <|> "\\("
    <|> "-size"
    <|> "1920x1080"
    <|> "xc:transparent"
    <|> "-font"
    <|> "\"EB-Garamond-Regular\""
    <|> "-pointsize"
    <|> show font_size
    <|> "-fill"
    <|> "black"
    <|> "-draw"
    <|> "\"text"
    <|> "0,0"
    <|> ("\'" <> T.unpack (quoteEscape False quoteStr) <> "\'" <> "\"")
    <|> "-blur"
    <|> "0x2"
    <|> "\\)"
    <|> "-geometry +3+3 -composite"

    <|> "\\("
    <|> "-blur"
    <|> "0x0"
    <|> "-font"
    <|> "EB-Garamond-Regular"
    <|> "-fill"
    <|> "white"
    <|> "-pointsize"
    <|> show font_size
    <|> "-annotate"
    <|> "+0+0"
    <|> ("\'" <> T.unpack (quoteEscape True quoteStr)  <> "\'")
    <|> "\\)"

    <|> "-font"
    <|> "EB-Garamond-Italic"
    <|> "-pointsize"
    <|> show font_size
    <|> noteStr

    <|> outDir
  return outFile
