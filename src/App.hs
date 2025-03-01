-- No clown T.pack and T.unpack
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

module App
  ( Env(..)
  , ApiKey(fromApiKey)
  , toApiKey
  , App(..)
  ) where

import qualified Data.Text.Lazy as T
import Control.Monad.Reader (ReaderT, MonadIO)

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

data App a = forall m. MonadIO m => App {runApp :: ReaderT Env m a}
