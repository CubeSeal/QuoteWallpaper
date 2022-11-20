module UsefulFunctions
  ( (!?)
  , safeHead
  , safeTail
  , safeLast
  , getISODate ) where

import Control.Monad.IO.Class (MonadIO (liftIO))

import qualified Data.Time.Clock as C (UTCTime (utctDay), getCurrentTime)
import qualified Data.Time.Format.ISO8601 as C (iso8601Show)

-- A blight on the language smh.
infix 5 !?

(!?) :: [a] -> Int -> Maybe a
(!?) []     _ = Nothing
(!?) (x:_)  0 = Just x
(!?) (_:xs) n = xs !? (n - 1)

safeHead :: [a] -> Maybe a
safeHead = (!? 0)

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:as) = Just as

safeLast :: [a] -> Maybe a
safeLast []     = Nothing
safeLast x      = Just $ last x

getISODate :: MonadIO m => m String
getISODate =  liftIO $ C.iso8601Show . C.utctDay <$> C.getCurrentTime