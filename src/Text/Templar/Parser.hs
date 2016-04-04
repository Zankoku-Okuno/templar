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
parseTemplate cfg = Sequence <$> many (parseText cfg <|> parseTag cfg)


parseText :: Parse Template
parseText = (Literal <$>) . takeTillString . cfgStartTag

parseTag :: Parse Template
parseTag = tagBoilerplate parseOutput

tagBoilerplate :: Parser a -> Parse a
tagBoilerplate inner (Config {..}) = do
    string cfgStartTag
    it <- inner
    --TODO comment
    --TODO trim whitespace?
    string cfgEndTag
    pure it


parseOutput :: Parser Template
parseOutput = do
    ctor <- option Relative (Rooted <$ char '.')
    --TODO different filter
    Output <$> parseSource

--TODO parseCond
    --TODO parseTruthy
    --TODO parseFalsey
--TODO parseLoop
--TODO parseEndBlock


parseSource :: Parser Source
parseSource = do
    f <- parseChain
    args <- many $ takeWhile1 (inClass " \t") >> parseChain
    pure $ Source f args

parseChain :: Parser Chain
parseChain = parseRooted <|> parseRelative <|> parseImmediate
    where
    parseRooted = Rooted <$> (char '.' >> parseLongName)
    parseRelative = Relative <$> parseLongName
    parseImmediate = Immediate <$> fail "TODO: immediate data unimplemented"
    parseLongName = do
        hd <- NameField <$> parseName -- TODO? allow # to start a chain
        tl <- many parseField
        pure $ hd:tl

parseField :: Parser Field
parseField = parseNameField <|> parseCountField
    where
    parseNameField = NameField <$> (char '.' >> parseName)
    parseCountField = CountField <$ char '#'

parseName :: Parser Name
parseName = T.unpack <$> takeWhile (inClass "a-zA-Z0-9_")


takeTillString :: Text -> Parser Text
takeTillString stopStr = parse
    where
    parse = do
        first <- takeTill (== stopChar)
        when (T.null first) $ fail "empty text"
        rest <- peekStopStr <|> recurse <|> pure ""
        pure $ first <> rest
    peekStopStr = lookAhead (string stopStr) >> pure ""
    recurse = do
        char <- T.singleton <$> char stopChar
        rest <- parse
        pure $ char <> rest
    stopChar = T.head stopStr