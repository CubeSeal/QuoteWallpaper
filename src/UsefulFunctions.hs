module UsefulFunctions
  ( (!?)
  , safeHead
  , safeTail
  , safeLast
  , getISODate
  , withReadFile ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import GHC.IO.Exception (IOException(IOError), IOErrorType (NoSuchThing))
import Control.Exception (try, throwIO)

import qualified Data.Time.Clock as C (UTCTime (utctDay), getCurrentTime)
import qualified Data.Time.Format.ISO8601 as C (iso8601Show)

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
getISODate :: MonadIO m => m String
getISODate =  liftIO $ C.iso8601Show . C.utctDay <$> C.getCurrentTime

-- | Safe readFile function that allows for failure path.
withReadFile :: MonadIO m => FilePath -> m a -> (String -> m a) -> m a
withReadFile filepath failIO successIO = do
  fileRaw <- liftIO . try $ readFile filepath

  case fileRaw of
    Left (IOError _ NoSuchThing _ _ _ _) -> failIO
    Left e -> liftIO $ throwIO e
    Right fileText -> successIO fileText