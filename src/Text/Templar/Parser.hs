{-#LANGUAGE OverloadedStrings, RecordWildCards #-}
module Text.Templar.Parser where

import Prelude hiding (takeWhile)

import Text.Templar.Syntax

import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid
import Control.Applicative
import Control.Monad.Reader
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator


type Parse a = Config -> (Parser a)

runParser :: Config -> Text -> Either String Template
runParser cfg = parseOnly (parseTemplate cfg)

parseTemplate :: Parse Template
parseTemplate cfg = do
    initial <- many (parseText cfg <|> parseTag cfg)
    last <- Literal <$> takeText
    pure $ Sequence (initial ++ [last])

parseText :: Parse Template
parseText = (Literal <$>) . parse . cfgStartTag
    where
    parse startTag = do
        first <- takeTill (== startChar)
        when (T.null first) $ fail "empty text"
        rest <- peekStartTag <|> recurse
        pure $ first <> rest
        where
        peekStartTag = lookAhead (string startTag) >> pure ""
        recurse = do
            char <- T.singleton <$> char startChar
            rest <- parse startTag
            pure $ char <> rest
        startChar = T.head startTag

parseTag :: Parse Template
parseTag (Config {..}) = do
    string cfgStartTag
    template <- parseOutput
    string cfgEndTag
    pure template

parseOutput :: Parser Template
parseOutput = do
    name <- parseName
    pure $ Output (Source (Relative [NameField name]) [])

parseName :: Parser Name
parseName = T.unpack <$> takeWhile (inClass "a-zA-Z0-9_")