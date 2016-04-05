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


type Parse a = Config -> Parser a

runParser :: Config -> Text -> Either String Template
runParser cfg = parseOnly (parseTemplate cfg)

parseTemplate :: Parse Template
parseTemplate cfg = parseBlock cfg <* endOfInput


parseBlock :: Parse Template
parseBlock cfg = Sequence <$> many (parseText cfg <|> parseTag cfg)

parseText :: Parse Template
parseText = (Literal <$>) . takeTillString . cfgStartTag

parseTag :: Parse Template
parseTag cfg = parseOutput cfg <|> parseCond cfg <|> parseLoop cfg


tagBoilerplate :: Parser a -> Parse a
tagBoilerplate inner (Config {..}) = do
    string cfgStartTag
    it <- inner
    --TODO whitespace just inside tag braces
    --TODO comment
    --TODO trim whitespace?
    string cfgEndTag
    pure it


parseOutput :: Parse Template
parseOutput = tagBoilerplate $ do
    --TODO different filter
    Output <$> parseSource

parseCond :: Parse Template
parseCond cfg = do
    first <- parseIf
    elifs <- many parseElif
    last <- option (Literal "") parseElse
    parseEndBlock cfg
    pure $ CondBlock (first : elifs) last
    where
    parseIf = parseClause False
    parseElif = parseClause True
    parseElse = do
        tagBoilerplate (char '|') cfg --FIXME not just standard tag boilerblate, but allow anything after the slash
        parseBlock cfg
    parseClause elif = do
        (expected, predicate) <- flip tagBoilerplate cfg $ do
            when elif $ () <$ char '|'
            expected <- (True <$ char '?') <|> (False <$ char '!')
            predicate <- parseSource
            pure (expected, predicate)
        block <- parseBlock cfg
        pure $ (expected, predicate, block)

parseLoop :: Parse Template
parseLoop cfg = do
    (loopName, loopVar, loopOver) <- flip tagBoilerplate cfg $ do
        char '#'
        loopName <- optional (parseName <* string ": ") --FIXME more flexible whitespace
        loopVar <- optional (parseName <* string " <- ") --FIXME more flexible whitespace
        loopOver <- parseSource
        pure (loopName, loopVar, loopOver)
    loopBody <- parseBlock cfg
    loopEmpty <- option (Literal "") $ do
        tagBoilerplate (char '|') cfg
        parseBlock cfg
    parseEndBlock cfg
    pure $ LoopBlock {..}

parseEndBlock :: Parse ()
parseEndBlock = tagBoilerplate (() <$ char '/') --FIXME not just standard tag boilerblate, but allow anything after the slash


parseSource :: Parser Source
parseSource = do
    f <- parseChain
    args <- many $ takeWhile1 (inClass " \t") >> parseChain
    pure $ Source f args

parseChain :: Parser Chain
parseChain = parseRooted <|> parseRelative <|> parseImmediate
    where
    parseRooted = Rooted <$> (char '.' >> option [] parseLongName)
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
parseName = T.unpack <$> takeWhile1 (inClass "a-zA-Z0-9_")


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