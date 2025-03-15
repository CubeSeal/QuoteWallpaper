{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Commands.KDE
  ( setWallpaper
  , createImageFile
  ) where

-- Modules
import System.Process (callProcess, callCommand)
import Control.Monad.Reader
  ( ReaderT
  , MonadReader (ask)
  , MonadIO (liftIO)
  )

import qualified Data.Text.Lazy as T

import UsefulFunctions (getISODate)
import App(Env(..))

import qualified Clippings as C

-- | Set Wallpaper
setWallpaper :: MonadIO m => FilePath -> ReaderT Env m ()
setWallpaper fp = do
  Env dir _ <- ask
  let picDir = dir ++ "outfiles/" ++ fp
  liftIO $ callProcess "plasma-apply-wallpaperimage" [picDir]

-- | Make image file
createImageFile
  :: MonadIO m
  => FilePath
  -> C.AnnotatedQuote
  -> ReaderT Env m FilePath
createImageFile inImgFile C.AQuote {..} = do
  Env dir _ <- ask
  date <- getISODate
  let
    infixl 5 <|>
    (<|>) x y = x <> " " <> y
    picDir   = dir ++ "infiles/" ++ inImgFile
    outFile  = "out-" ++ date ++ ".jpg"
    outDir   = dir ++ "outfiles/" ++ outFile
    quoteStr = T.unpack
      $ T.replace "\"" "\\\"" aQuote
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
    <|> "40"
    <|> "-fill"
    <|> "black"
    <|> "-draw"
    <|> "\"text"
    <|> "0,0"
    <|> ("\'" <> quoteStr <> "\'" <> "\"")
    <|> "-blur"
    <|> "0x4"
    <|> "\\)"
    <|> "-geometry +5+5 -composite"

    <|> "\\("
    <|> "-blur"
    <|> "0x0"
    <|> "-font"
    <|> "EB-Garamond-Regular"
    <|> "-fill"
    <|> "white"
    <|> "-pointsize"
    <|> "40"
    <|> "-annotate"
    <|> "+0+0"
    <|> ("\'" <> quoteStr <> "\'")
    <|> "\\)"

    <|> "-font"
    <|> "EB-Garamond-Italic"
    <|> "-pointsize"
    <|> "40"
    <|> noteStr

    <|> outDir
  return outFile
