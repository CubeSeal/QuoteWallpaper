{-# LANGUAGE OverloadedStrings #-}

module Clippings (NoteType, AnnotatedQuote(..), rawToAQuotes) where

-- Modules
import Data.Char       (isLetter)
import Data.Maybe      (mapMaybe, fromMaybe)
import UsefulFunctions (safeLast, (!?))

import qualified Data.Time as C
import qualified Data.Text.Lazy  as T
import Text.Read (readMaybe)

-- Some useful datatypes
-- | Simple sum type to mark whether quote is a note/highlight.
data NoteType
  = Note
  | Highlight
  deriving (Show, Eq)

-- | Original quote type parsed directly into.
data Quote = Quote
  { author   :: T.Text
  , book     :: T.Text
  , noteType :: NoteType
  , quote    :: T.Text
  , datetime :: C.LocalTime
  } deriving (Show, Eq)

-- | Informative quote type that associates notes with highlights properly.
data AnnotatedQuote = AQuote
  { aAuthor :: T.Text
  , aBook   :: T.Text
  , aQuote  :: T.Text
  , aNote   :: Maybe T.Text
  , aDateTime :: C.LocalTime
  } deriving (Show)

-- | Get Valid Quotes from Raw file data.
rawToAQuotes :: T.Text -> [AnnotatedQuote]
rawToAQuotes = quotesToAQuotes . rawToQuotes

rawToQuotes :: T.Text -> [Quote]
rawToQuotes = mapMaybe parseRawQuote . T.splitOn delim
  where
    delim = "=========="

-- | Conversion function between quote types.
quotesToAQuotes :: [Quote] -> [AnnotatedQuote]
quotesToAQuotes [] = []
quotesToAQuotes (Quote auth bk Highlight qte dte : Quote _ _ Note note _: xs) =
  AQuote auth bk qte (Just note) dte : quotesToAQuotes xs
quotesToAQuotes (Quote a b _ q d: xs) = AQuote a b q Nothing d : quotesToAQuotes xs

-- | Parse Quote from file text.
parseRawQuote :: T.Text -> Maybe Quote
parseRawQuote str = do
  let ls = T.lines $ T.dropWhile isNewline str
  fstLine <- ls !? 0
  sndLine <- ls !? 1
  getQuote <- T.dropAround isNewline <$> safeLast ls
  return $ Quote
    { author = getAuthor fstLine
    , book = getBook fstLine
    , noteType = getNoteType sndLine
    , quote = getQuote
    , datetime = getDateTime sndLine
    }

-- | Parse datetime
getDateTime :: T.Text -> C.LocalTime
getDateTime txt = C.LocalTime date time
  where
    timeTxt = T.words . T.takeWhileEnd (`notElem` [',']) $ txt
    date = fromMaybe (C.fromGregorian 2024 C.October 7  ) $ do
        day <- timeTxt !? 0 >>= tRead
        month <- timeTxt !? 1 >>= monthParse
        year <- timeTxt !? 2 >>= tRead

        return $ C.fromGregorian year month day
    time = fromMaybe C.midnight $ timeTxt !? 3 >>= tRead
    tRead :: Read a => T.Text -> Maybe a
    tRead = readMaybe . T.unpack
    monthParse :: T.Text -> Maybe C.MonthOfYear
    monthParse "January" = Just C.January
    monthParse "February" = Just C.February
    monthParse "March" = Just C.March
    monthParse "April" = Just C.April
    monthParse "May" = Just C.May
    monthParse "June" = Just C.June
    monthParse "July" = Just C.July
    monthParse "August" = Just C.August
    monthParse "September" = Just C.September
    monthParse "October" = Just C.October
    monthParse "November" = Just C.November
    monthParse "December" = Just C.December
    monthParse _ = Nothing

getNoteType :: T.Text -> NoteType
getNoteType txt
  | "Your Note" `T.isInfixOf` txt = Note
  | otherwise                     = Highlight

getAuthor :: T.Text -> T.Text
getAuthor = convertAuthor . dropSideNonLetters . snd . T.breakOnEnd "("
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
