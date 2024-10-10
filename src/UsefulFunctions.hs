-- No clown T.pack and T.unpack
{-# LANGUAGE OverloadedStrings #-}

module UsefulFunctions
  ( (!?)
  , safeHead
  , safeTail
  , safeLast
  , getISODate
  , withReadFile
  , getDay
  , replaceElems
  ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import GHC.IO.Exception (IOException(IOError), IOErrorType (NoSuchThing))
import Control.Exception (try, throwIO)
import System.Exit (exitSuccess)

import qualified Data.Time as C
import qualified Data.Text.Lazy as T
import qualified Data.Time.Format.ISO8601 as C

-- A blight on the language smh.
infix 5 !?

-- | Operator that performs lookup inside Maybe.
(!?) :: [a] -> Int -> Maybe a
(!?) []     _ = Nothing
(!?) (x:_)  0 = Just x
(!?) (_:xs) n = xs !? (n - 1)

-- | Non-partial head.
safeHead :: [a] -> Maybe a
safeHead = (!? 0)

-- | Non-partial tail.
safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:as) = Just as

-- | Non-partial last.
safeLast :: [a] -> Maybe a
safeLast []     = Nothing
safeLast x      = Just $ last x

-- | Helper function that returns ISO Date.
getDay :: MonadIO m => m C.Day
getDay =  liftIO $ do
  C.ZonedTime (C.LocalTime day _ ) _ <- C.getZonedTime

  return day

-- | Helper function that returns ISO Date.
getISODate :: MonadIO m => m String
getISODate =  C.iso8601Show <$> getDay

-- | Safe readFile function that has failure path.
withReadFile :: MonadIO m => FilePath -> (String -> m a) -> m a
withReadFile filepath successIO = do
  fileRaw <- liftIO . try $ readFile filepath

  case fileRaw of
    Left (IOError _ NoSuchThing _ _ _ _) -> do
      liftIO . putStrLn $ "Add " <> filepath <> " to directory." 
      liftIO exitSuccess
    Left e -> liftIO $ throwIO e
    Right fileText -> successIO fileText

-- | Replace multiple elems in Text
replaceElems :: [(T.Text, T.Text)] -> T.Text -> T.Text
replaceElems elemReplacementList initialText = foldl f initialText elemReplacementList
   where
    f :: T.Text -> (T.Text, T.Text) -> T.Text
    f inputText (elem', replacement) = T.replace elem' replacement inputText