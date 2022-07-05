module UsefulFunctions
  ( safeHead
  , safeTail
  , safeLast
  , getISODate ) where

import qualified Data.Time.Clock as C (UTCTime (utctDay), getCurrentTime)
import qualified Data.Time.Format.ISO8601 as C (iso8601Show)

-- A blight on the language smh.
safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (a:_)  = Just a

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:as) = Just as

safeLast :: [a] -> Maybe a
safeLast []     = Nothing
safeLast x      = Just $ last x

getISODate :: IO String
getISODate =  C.iso8601Show . C.utctDay <$> C.getCurrentTime