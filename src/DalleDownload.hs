{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DalleDownload
  (
    fetchDalle3
  ) where

-- Modules
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.ByteString.Lazy (toStrict)
import Control.Monad.Reader (MonadIO)
import Network.HTTP.Req (runReq, defaultHttpConfig, POST (POST), (/:), jsonResponse, ReqBodyJson (ReqBodyJson), https, req, responseBody, header)
import GHC.Generics (Generic)
import Data.Maybe (fromMaybe)
import Data.Vector ((!?))

import UsefulFunctions (ApiKey (ApiKey))

import qualified Data.Text.Lazy as T
import qualified Data.Aeson as H
import qualified Data.Aeson.KeyMap as H

import qualified Clippings as C

-- Types
type URL = T.Text

-- | JSON format OpenAi API wants to receive info in.
data OpenAiJsonQuery = OpenAiJsonQuery
  { model :: T.Text
  , prompt :: T.Text
  , n :: Int
  , size :: T.Text
  } deriving (Show, Generic)

instance H.ToJSON OpenAiJsonQuery
instance H.FromJSON OpenAiJsonQuery

-- Functions
-- | Generate Dalle3 Image and return URL
fetchDalle3 :: MonadIO m => C.AnnotatedQuote -> ApiKey ->  m URL
fetchDalle3 C.AQuote {..} (ApiKey bearer)=
  runReq defaultHttpConfig $ do
    let
      bearerMsg = "Bearer " <> toStrict (encodeUtf8 bearer)
      testMessage = OpenAiJsonQuery
        "dall-e-3"
        ( "Draw a picture inspired by the following quote:"
        <> aQuote
        <> "Don't include the quote text in the picture."
        )
        1
        "1024x1024"
      url = https "api.openai.com" /: "v1" /: "images" /: "generations"
      params = header "Content-Type" "application/json"
        <> header "Authorization" bearerMsg

    _jsonResponse <- req POST url (ReqBodyJson testMessage) jsonResponse params
    return $ fromMaybe undefined $ extractURL $ responseBody _jsonResponse

extractURL :: H.Value -> Maybe URL
extractURL val = do
  (H.String url)
    <- objExtract "data" val
    >>= arrayExtract 0
    >>= objExtract "url"
  return $ T.fromStrict url

-- Helper functions for JSON extraction:

objExtract :: H.Key -> H.Value -> Maybe H.Value
objExtract key (H.Object x) = H.lookup key x
objExtract _   _            = Nothing

arrayExtract :: Int -> H.Value -> Maybe H.Value
arrayExtract index (H.Array x) = x !? index
arrayExtract _     _           = Nothing