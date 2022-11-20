{-# LANGUAGE OverloadedStrings #-}

module WikimediaAPI (fetchPOTD, URL) where

-- Modules
import Control.Monad.IO.Class ( MonadIO )
import Data.Vector ( (!?) )
import Data.Maybe (fromMaybe)
import Network.HTTP.Req
  ( (/:)
  , (=:)
  , defaultHttpConfig
  , https
  , jsonResponse
  , req
  , responseBody
  , runReq
  , GET(GET)
  , NoReqBody(NoReqBody)
  , Url
  , JsonResponse
  , Req
  )

import UsefulFunctions ( safeHead, getISODate )

import qualified Data.Aeson as H
import qualified Data.Aeson.KeyMap as H
import qualified Data.Text as T

-- Types
type FileName = T.Text
type URL      = T.Text

-- Functions
-- Make request to server for file name and then for url
fetchPOTD :: MonadIO m => m URL
fetchPOTD = do
  date <- T.pack <$> getISODate
  runReq defaultHttpConfig $ do
    let
      title  = "Template:Potd/" <> date <> " (en)"
      params =  "action"        =: ("query"  :: T.Text)
             <> "format"        =: ("json"   :: T.Text) 
             <> "formatversion" =: ("2"      :: T.Text)
             <> "prop"          =: ("images" :: T.Text)
             <> "titles"        =: title
      url    = https "commons.wikimedia.org" /: "w" /: "api.php"

    _jsonResponse1 <- req GET url NoReqBody jsonResponse params
    let imgSrc = fromMaybe undefined $ parseFileName $ responseBody _jsonResponse1

    _jsonResponse2 <- fetchImageSrc imgSrc url
    let imgUrl = fromMaybe undefined $ parseURL $ responseBody _jsonResponse2
    
    return imgUrl

-- Drop down JSON Object to extract filename
parseFileName :: H.Value -> Maybe T.Text
parseFileName x = do
  (H.String filename)
    <-  objExtract "query" x
    >>= objExtract "pages"  
    >>= arrayExtract 0       
    >>= objExtract "images"  
    >>= arrayExtract 0       
    >>= objExtract "title"
  return filename

-- Make another request to Wikimedia API for file URL
fetchImageSrc :: FileName -> Url a -> Req (JsonResponse H.Value)
fetchImageSrc f url = do
  let
    params =  "action" =: ("query"     :: T.Text)
           <> "format" =: ("json"      :: T.Text)
           <> "prop"   =: ("imageinfo" :: T.Text)
           <> "iiprop" =: ("url"       :: T.Text)
           <> "titles" =: f
  req GET url NoReqBody jsonResponse params

-- Drop down the JSON object to extract url
parseURL :: H.Value -> Maybe URL
parseURL x = do
  (H.String url)
    <-  objExtract "query" x
    >>= objExtract "pages"     
    >>= f                      
    >>= objExtract "imageinfo" 
    >>= arrayExtract 0         
    >>= objExtract "url"
  return url
  where
    f :: H.Value -> Maybe H.Value
    f (H.Object x') = (safeHead . H.elems) x'
    f _             = Nothing

-- Helper functions for JSON extraction:

objExtract :: H.Key -> H.Value -> Maybe H.Value
objExtract key (H.Object x) = H.lookup key x
objExtract _   _            = Nothing

arrayExtract :: Int -> H.Value -> Maybe H.Value
arrayExtract index (H.Array x) = x !? index
arrayExtract _     _           = Nothing