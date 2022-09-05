{-# LANGUAGE OverloadedStrings #-}

module Clippings
  ( NoteType
  , Quote (..)
  , rawToQuotes ) where

-- Modules
import Data.Maybe (mapMaybe)
import Data.Char
  ( isLetter
  , isPunctuation
  )

import UsefulFunctions
  ( (!?)
  , safeLast
  )
  
import qualified Data.Text.Lazy as T

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
  . mapMaybe parseRawQuote
  . T.splitOn delim
  where
    delim = "=========="

-- Parse Quote
parseRawQuote :: T.Text -> Maybe Quote
parseRawQuote str = do
  let ls = T.lines $ T.dropWhile isNewline str
  fstLine  <- ls !? 0
  sndLine  <- ls !? 1
  getQuote <- T.dropAround isNewline <$> safeLast ls
  return
    $ Quote { author    = getAuthor fstLine
            , book      = getBook fstLine
            , noteType  = getNoteType sndLine
            , quote     = getQuote }

getNoteType :: T.Text -> NoteType
getNoteType txt = if "Your Note" `T.isInfixOf` txt then Note else Highlight

getAuthor :: T.Text -> T.Text
getAuthor = convertAuthor
  . dropSideNonLetters
  . snd
  . T.breakOnEnd "("
  where
    convertAuthor str' = case T.breakOn ", " str' of
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
filterQuote Quote { author = a
                  , quote = q
                  , noteType = n } =
  p1 && p2 && p3 && p4
  where
    p1 = n == Highlight
    p2 = maybe False (isPunctuation . snd) $ T.unsnoc q
    p3 = all (`notElem` T.words a) ["Kenneth", "Fred"]
    p4 = length (T.words q) > 1