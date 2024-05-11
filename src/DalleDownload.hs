{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module DalleDownload 
  (
    testConnection
  ) where

import Control.Monad.Reader (MonadIO)
import Network.HTTP.Req (runReq, defaultHttpConfig, POST (POST), (/:), jsonResponse, ReqBodyJson (ReqBodyJson), https, req, responseBody, header)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON, Value)

-- | List format OpenAi API wants to receive info in.
data OpenAiMessage = OpenAiMessage
  { role :: String
  , content :: String
  } deriving (Show, Generic)

instance ToJSON OpenAiMessage
instance FromJSON OpenAiMessage

-- | JSON format OpenAi API wants to receive info in.
data OpenAiJsonQuery = OpenAiJsonQuery
  { model :: String
  , messages :: [OpenAiMessage]
  } deriving (Show, Generic)

instance ToJSON OpenAiJsonQuery
instance FromJSON OpenAiJsonQuery

-- | Simple function to test connection to OpenAi server
testConnection :: MonadIO m => m String
testConnection = 
  runReq defaultHttpConfig $ do
    let
      testMessage = OpenAiJsonQuery "gpt-3.5-turbo"
        [ OpenAiMessage "system" "You are a poetic assistant, skilled in explaining complex programming concepts with creative flair."
        , OpenAiMessage "user" "Compose a poem that explains the concept of recursion in programming."
        ]
      params =  header "Content-Type" "application/json"
             <> header "Authorization" "Bearer"
      url    = https "api.openai.com" /: "v1" /: "chat" /: "completions"
    
    _jsonResponse <- req POST url (ReqBodyJson testMessage) jsonResponse params
    return $ show (responseBody _jsonResponse :: Value)