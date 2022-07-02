{-# LANGUAGE OverloadedStrings #-}

module WikimediaAPI (fetchPOTD, URL) where

-- Modules
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
  , JsonResponse )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Data.Vector ( (!?) )
import Data.Maybe (fromMaybe)

import qualified Data.Aeson as H ( Value (..), Key)
import qualified Data.Aeson.KeyMap as H (lookup)
import qualified Data.Time.Format.ISO8601 as C (iso8601Show)
import qualified Data.Time.Clock as C (getCurrentTime, UTCTime (utctDay))
import qualified Data.Text as T

-- Types
type FileName = T.Text
type URL = T.Text

-- Functions
fetchPOTD :: IO URL
fetchPOTD = do
  date <- T.pack <$> getISODate
  runReq defaultHttpConfig $ do
    let
      title = "Template:Potd/" <> date <> " (en)"
      params = "action" =: ("query" :: T.Text)    <>
               "format" =: ("json" :: T.Text)     <>
               "formatversion" =: ("2" :: T.Text) <>
               "prop" =: ("images" :: T.Text)     <>
               "titles" =: title
      url = https "commons.wikimedia.org" /: "w" /: "api.php"
    bs <- req GET url NoReqBody jsonResponse params
    let imgSrc = fromMaybe undefined $ parseFileName $ responseBody bs
    bs2 <- liftIO $ fetchImageSrc imgSrc url
    let imgUrl = fromMaybe undefined $ parseURL $ responseBody bs2
    liftIO $ return imgUrl

getISODate :: IO String
getISODate =  C.iso8601Show . C.utctDay <$> C.getCurrentTime

parseFileName :: H.Value -> Maybe T.Text
parseFileName x = do
  (H.String filename) <- objExtract "query" x >>=
    objExtract "pages" >>=
      arrayExtract 0 >>=
        objExtract "images" >>=
          arrayExtract 0 >>=
            objExtract "title"
  return filename

fetchImageSrc :: FileName -> Url a -> IO (JsonResponse H.Value)
fetchImageSrc f url = runReq defaultHttpConfig $ do
  let
    params = "action" =: ("query" :: T.Text)    <>
             "format" =: ("json" :: T.Text)     <>
             "prop" =: ("imageinfo" :: T.Text)     <>
             "iiprop" =: ("url" :: T.Text) <>
             "titles" =: f
  req GET url NoReqBody jsonResponse params

parseURL :: H.Value -> Maybe URL
parseURL x = do
  (H.String url) <- objExtract "query" x >>=
    objExtract "pages" >>=
      objExtract "85032370" >>=
        objExtract "imageinfo" >>=
          arrayExtract 0 >>=
            objExtract "url"
  return url

objExtract :: H.Key -> H.Value -> Maybe H.Value
objExtract key (H.Object x) = H.lookup key x
objExtract _ _ = Nothing

arrayExtract :: Int -> H.Value -> Maybe H.Value
arrayExtract index (H.Array x) = x !? index
arrayExtract _ _ = Nothing