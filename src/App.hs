-- No clown T.pack and T.unpack
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module App
  ( Env(..)
  , ApiKey(fromApiKey)
  , toApiKey
  ) where

import qualified Data.Text.Lazy as T

-- Data types
-- | Enviornment data type
-- Ugh
data Env = Env
  { directory :: !FilePath
  , apiKey :: !ApiKey
  }

-- | ApiKey Wrapper
newtype ApiKey = ApiKey {fromApiKey :: T.Text}

toApiKey :: String -> ApiKey
toApiKey = ApiKey . T.replace "\n" mempty . T.pack 
