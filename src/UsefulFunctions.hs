module UsefulFunctions
  ( safeHead
  , safeTail
  , safeLast ) where

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