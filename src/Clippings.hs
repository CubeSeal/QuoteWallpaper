{-# LANGUAGE OverloadedStrings #-}

module Clippings (NoteType, AnnotatedQuote(..), rawToAQuotes) where

-- Modules
import           Data.Char       (isLetter, isPunctuation)
import           Data.Maybe      (mapMaybe)
import qualified Data.Text.Lazy  as T
import           UsefulFunctions (safeLast, (!?))

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
  } deriving (Show, Eq)

-- | Informative quote type that associates notes with highlights properly.
data AnnotatedQuote = AQuote
  { aAuthor :: T.Text
  , aBook   :: T.Text
  , aQuote  :: T.Text
  , aNote   :: Maybe T.Text
  } deriving (Show)

-- | Get Valid Quotes from Raw file data.
rawToAQuotes :: T.Text -> [AnnotatedQuote]
rawToAQuotes = filter filterAQuote . quotesToAQuotes . rawToQuotes

rawToQuotes :: T.Text -> [Quote]
rawToQuotes = mapMaybe parseRawQuote . T.splitOn delim
  where
    delim = "=========="

-- | Conversion function between quote types.
quotesToAQuotes :: [Quote] -> [AnnotatedQuote]
quotesToAQuotes [] = []
quotesToAQuotes (Quote auth bk Highlight qte : Quote _ _ Note note: xs) =
  AQuote auth bk qte (Just note) : quotesToAQuotes xs
quotesToAQuotes (Quote a b _ q : xs) = AQuote a b q Nothing : quotesToAQuotes xs

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
    }

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

filterAQuote :: AnnotatedQuote -> Bool
filterAQuote AQuote
  { aAuthor = a
  , aQuote = q
  } = p1 && p2 && p3
  where
    p1 = maybe False (isPunctuation . snd) $ T.unsnoc q
    p2 = all (`notElem` T.words a) ["Kenneth", "Fred"]
    p3 = length (T.words q) > 1
