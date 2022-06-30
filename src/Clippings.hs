
{-# LANGUAGE OverloadedStrings #-}

module Clippings (NoteType, Quote (..), rawToQuotes) where

-- Modules
import qualified Data.Text.Lazy as T
import Data.Maybe (mapMaybe)
import Data.Char (isLetter, isPunctuation)

-- Some useful datatypes
data NoteType = Note | Highlight
  deriving (Show, Eq)

data Quote = Quote
  { author    :: T.Text
  , book      :: T.Text
  , noteType  :: NoteType
  , quote     :: T.Text }
  deriving (Show, Eq)

  -- Get Valid Quotes from Raw file data
rawToQuotes :: T.Text -> [Quote]
rawToQuotes = filter filterQuote
  . mapMaybe getQuote
  . T.splitOn delim
  where
    delim = "=========="

-- Parse Quote
getQuote :: T.Text -> Maybe Quote
getQuote str = do
  let ls = T.lines $ T.dropWhile isNewline str
  fstLine  <- safeHead ls
  sndLine  <- safeTail ls >>= safeHead
  getQuote <- T.dropAround isNewline <$> safeLast ls
  return
    $ Quote { author    = getAuthor fstLine
            , book      = getBook fstLine
            , noteType  = getNoteType sndLine
            , quote     = getQuote }
  where
    -- A blight on the language smh.
    safeHead []     = Nothing
    safeHead (a:as) = Just a
    safeTail []     = Nothing
    safeTail (a:as) = Just as
    safeLast []     = Nothing
    safeLast x      = Just $ last x

getNoteType :: T.Text -> NoteType
getNoteType txt = if "Your Note" `T.isInfixOf` txt then Note else Highlight

getAuthor :: T.Text -> T.Text
getAuthor str = convertAuthor
  . dropSideNonLetters
  . snd
  . T.breakOnEnd "("
  $ str
  where
    convertAuthor str = case T.breakOn ", " str of
      (x, y) -> dropSideNonLetters $ y <> " " <> x

dropSideNonLetters :: T.Text -> T.Text
dropSideNonLetters = T.dropAround (not . isLetter)

isParenthesis :: Char -> Bool
isParenthesis = (`elem` ['(', ')'])

isNewline :: Char -> Bool
isNewline = (`elem` ['\r', '\n'])

getBook :: T.Text -> T.Text
getBook = dropSideNonLetters
  . T.takeWhile (not . isParenthesis)
  . T.dropWhile (not . isLetter)

filterQuote :: Quote -> Bool
filterQuote Quote{author = a, quote = q} = maybe False and (sequence [p2, p3])
  where
    p2 = isPunctuation . snd <$> T.unsnoc q
    p3 = Just $ "Kenneth" `notElem` T.words a