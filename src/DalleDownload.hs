{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DalleDownload
  (
    fetchDalle3
  ) where

-- Modules
import Data.Text.Encoding (encodeUtf8)
import Control.Monad.Reader (MonadIO)
import Network.HTTP.Req
  ( runReq
  , defaultHttpConfig
  , POST (POST)
  , (/:)
  , jsonResponse
  , ReqBodyJson (ReqBodyJson)
  , https
  , req
  , responseBody
  , header
  )
import GHC.Generics (Generic)
import Data.Maybe (fromMaybe)
import Data.Vector ((!?))

import App (ApiKey (fromApiKey))

import qualified Data.Text.Lazy as T
import qualified Data.Aeson as H
import qualified Data.Aeson.KeyMap as H
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as BS

import qualified Clippings as C

-- Types
type Bytes = BS.ByteString

-- | Quality of the image.
data Quality = Low | Medium | High
    deriving (Generic)

-- | Manual show instance for lower case prints.
instance Show Quality where
  show Low = "low"
  show Medium = "medium"
  show High = "high"

instance H.ToJSON Quality where
    toJSON Low = H.String "low"
    toJSON Medium = H.String "medium"
    toJSON High = H.String "high"

instance H.FromJSON Quality where
    parseJSON (H.String "low") = return Low
    parseJSON (H.String "medium") = return Medium
    parseJSON (H.String "high") = return High
    parseJSON _ = fail "Invalid quality value"

-- | JSON format OpenAi API wants to receive info in.
data OpenAiJsonQuery = OpenAiJsonQuery
  { model :: T.Text
  , prompt :: T.Text
  , n :: Int
  , size :: T.Text
  , quality :: Quality
  } deriving (Show, Generic)

instance H.ToJSON OpenAiJsonQuery
instance H.FromJSON OpenAiJsonQuery

-- Functions
-- | Generate Dalle3 Image and return URL
fetchDalle3 :: MonadIO m => C.AnnotatedQuote -> ApiKey -> m Bytes
fetchDalle3 C.AQuote {..} apiKey =
  runReq defaultHttpConfig $ do
    let
      infixl 5 <|>
      (<|>) x y = x <> " " <> y
      bearerMsg = "Bearer " <> (encodeUtf8 . T.toStrict . fromApiKey) apiKey
      commentaryStr (Just str) = " And my note:" <|> str
      commentaryStr _ = mempty
      testMessage = OpenAiJsonQuery
        "gpt-image-1"
        ( "Draw a picture inspired by the following quote:"
        <|> "\""
        <|> aQuote
        <|> "\"."
        <|> "Please think carefully and take your time with generating this image."
        <|> "Make sure you don't accidentally put the text of the quote in the picture."
        <|> "Ensure that your picture fully embodies the meaning of the quote in terms of"
        <|> " artstyle and themes, taking into account subtext and hidden meaning."
        <|> "To help you I'll give you the author as well:"
        <|> aAuthor
        <|> ", the text which the quote is sourced from:"
        <|> aBook
        <|> commentaryStr aNote
        )
        1
        "1024x1024"
        Medium
      url = https "api.openai.com" /: "v1" /: "images" /: "generations"
      params = header "Content-Type" "application/json"
        <> header "Authorization" bearerMsg

    _jsonResponse <- req POST url (ReqBodyJson testMessage) jsonResponse params
    return $ fromMaybe undefined $ extractURL $ responseBody _jsonResponse

extractURL :: H.Value -> Maybe Bytes
extractURL val = do
  (H.String url)
    <- objExtract "data" val
    >>= arrayExtract 0
    >>= objExtract "b64_json"
  return $ B64.decodeLenient $ encodeUtf8 url

-- Helper functions for JSON extraction:

objExtract :: H.Key -> H.Value -> Maybe H.Value
objExtract key (H.Object x) = H.lookup key x
objExtract _   _            = Nothing

arrayExtract :: Int -> H.Value -> Maybe H.Value
arrayExtract index (H.Array x) = x !? index
arrayExtract _     _           = Nothing
