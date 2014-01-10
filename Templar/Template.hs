{-
most everything is text, except:
    #{<any>} comment
    \{<any>} escape/verbatim
    ${<name>(.<name>)*} replace with value
    @{[<index>], <var> in <name>(.<member>)*} loop
    ?{<condition>} conditional
        condition ::= <name>(.<member>)* check scalar non-null, vector non-empty
                   |  more to come? (int+bool+string arithmetic, function call)
    |{<condition'>}
        condition' ::= <condition>
                    |  else | empty

    %{<directive>}
        directive ::= extends <filename>
                   |  block <name>
                   |  more | super
                   |  more to come?
    &{<function> <arg>*}
        function calls are really just builtins, but we should be able to expand easily them w/ access to the templar source
        potentially, we could build an interpreter and hook a function up to it
        no matter what, function calls should be pure (except obviously, they must have read access to the input data, or config, or whatever)
        functions would be actually _defined_ in a totally separate file
within one of those, <any> is text, <name> and <function> must abide by fairly normal identifier rules, <index> and <var> were just descriptive synonyms for <name>, <member> ::= <name> | <int literal>
<arg> ::= <name> | <int literal> | <string literal>



location reporting isn't so important to me, but maybe I should try to learn how to build a lexer/parser in Parsec
we ought to be able to decide which names have what types based on their use
    given this, we'll create a Hajj for our data
    but also, we shuold be able to tell the user what is expected, and if something is inconsistent


lesser things:
!{<any>} report an error
={<name> <arg>} set some configuration, or bind a name
l10n & i18n
-}

module Templar.Template (
      Template
    , parse
    ) where

import Data.List
import Data.Char (digitToInt)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Control.Applicative ((<$>), (<*>), (*>), (<*))
import Control.Monad

import Text.Parsec hiding (token, satisfy, parse)
import qualified Text.Parsec as Parsec
import Text.Parsec.Text ()


------ Entry ------
--TODO getHajj :: TemplateSystem {-stack of Templates-} -> Hajj ?

--TODO render :: TemplateSystem -> Hajji ? -> Text

parse :: FilePath -> Text -> Either String (FilePath, Template)
parse file content = do
        tokens <- tokenize file content
        case runParser parseFile [] file tokens of
            Right val -> Right val
            Left err -> Left $ show err
    where
    parseFile = do
        super <- parseExtends
        content <- parseTemplate
        return (super, content)


------ Abstract Syntax ------
data Template = Raw Text
              | Scalar Access
              | Vector (Maybe Ident) Ident Access Template (Maybe Template)
              | Cond [(Condition, Template)] Template
              | Include FilePath
              | Block Ident Template
              | More Ident
              | Super Ident
              | Call Text [Argument]
data Condition = Have Access
                -- TODO MAYBE function call, int+bool+string arithmetic
data Command = ExtendsT FilePath
             | IncludeT FilePath
             | BlockT Ident
             | MoreT (Maybe Ident)
             | SuperT (Maybe Ident)
             | EndT
            -- TODO MAYBE what other directives could there be?
data Argument = Name Access
              | IntLit Integer
              | StrLit Text
              --TODO function call here again?

type Ident = Text
type Member = Either Integer Text
data Access = Access Text [Member]


------ Parsers ------
parseTemplate :: Parser Template
parseTemplate = choice [ parseText
                       , parseScalar
                       , parseVector
                       , parseCond
                       , parseInclude
                       , parseBlock
                       , parseMore
                       , parseSuper
                       , parseCall
                       ]

parseExtends :: Parser FilePath
parseExtends = extends <$> satisfy isExtends
    where
    isExtends (CommandT (ExtendsT _)) = True
    isExtends _ = False
    extends (CommandT (ExtendsT x)) = x


parseText :: Parser Template
parseText = merge <$> many1 (satisfy isText)
    where
    isText (RawT _) = True
    isText _ = False
    merge = Raw . T.concat . map unRawT
        where unRawT (RawT x) = x

parseScalar :: Parser Template
parseScalar = scalar <$> satisfy isScalar
    where
    isScalar (ScalarT _) = True
    isScalar _ = False
    scalar (ScalarT x) = Scalar x

parseVector :: Parser Template
parseVector = do
        (VectorT index var xs) <- satisfy isVector
        body <- parseTemplate
        empty <- optionMaybe parseElse
        return $ Vector index var xs body empty
    where
    isVector (VectorT _ _ _) = True
    isVector _ = False

parseCond :: Parser Template
parseCond = do
        (IfT test) <- satisfy isIf
        body <- parseTemplate
        elifs <- many parseElif
        other <- parseElse
        return $ Cond ((test, body):elifs) other
    where
    isIf (IfT _) = True
    isIf _ = False
    parseElif = do
            (ElifT test) <- satisfy isElif
            body <- parseTemplate
            return (test, body)
        where
        isElif (ElifT _) = True
        isElif _ = False

parseInclude :: Parser Template
parseInclude = include <$> satisfy isInclude
    where
    isInclude (CommandT (IncludeT _)) = True
    isInclude _ = False
    include (CommandT (IncludeT x)) = Include x

parseBlock :: Parser Template
parseBlock = do
        name <- block <$> satisfy isBlock
        body <- withBlock name parseTemplate
        parseEnd
        return $ Block name body
    where
    isBlock (CommandT (BlockT _)) = True
    isBlock _ = False
    block (CommandT (BlockT x)) = x

parseMore :: Parser Template
parseMore = more =<< satisfy isMore
    where
    isMore (CommandT (MoreT _)) = True
    isMore _ = False
    more (CommandT (MoreT (Just name))) = return $ More name
    more (CommandT (MoreT Nothing)) = More <$> currentBlock

parseSuper :: Parser Template
parseSuper = super =<< satisfy isSuper
    where
    isSuper (CommandT (SuperT _)) = True
    isSuper _ = False
    super (CommandT (SuperT (Just name))) = return $ Super name
    super (CommandT (SuperT Nothing)) = Super <$> currentBlock

parseCall :: Parser Template
parseCall = call <$> satisfy isCall
    where
    isCall (CallT _ _) = True
    isCall _ = False
    call (CallT f xs) = Call f xs


parseElse :: Parser Template
parseElse = between (satisfy isElse) parseEnd parseTemplate
    where
    isElse ElseT = True
    isElse _ = False

parseEnd :: Parser ()
parseEnd = void $ satisfy isEnd
    where
    isEnd (CommandT EndT) = True
    isEnd _ = False


type ParserState = [Ident]

withBlock :: Ident -> Parser a -> Parser a
withBlock name p = between pushBlock popBlock p
    where
    pushBlock = modifyState (name:)
    popBlock = modifyState (\s -> if null s then s else tail s)

currentBlock :: Parser Ident
currentBlock = do
    s <- getState
    if null s then parserZero else return (head s)


------ Tokenizer ------
data Token = RawT Text
           | ScalarT Access
           | VectorT (Maybe Ident) Ident Access
           | IfT Condition
           | ElifT Condition
           | ElseT
           | CommandT Command
           | CallT Text [Argument]
           | CommentT

tokenize :: FilePath -> Text -> Either String [Token]
tokenize file input = case Parsec.parse (many token) file input of
        Right val -> Right $ postProcess val
        Left err  -> Left  $ show err
    where
    postProcess :: [Token] -> [Token]
    postProcess = filter notComment
        where
        notComment CommentT = False
        notComment _ = True


------ Tokenizers ------
token :: Lexer Token
token = choice [ rawText
               , verbatimText
               , scalar
               , vector
               , conditional
               , additional
               , command
               , function
               , comment
               ]

rawText :: Lexer Token
rawText = RawT . pack <$> anyChar `manyTill` openDirective

verbatimText :: Lexer Token
verbatimText = RawT . pack <$> inDirective '\\' (anyChar `manyTill` char '}')

scalar :: Lexer Token
scalar = ScalarT <$> inDirective '$' access

vector :: Lexer Token
vector = do
    index <- optionMaybe $ try (tok ident << char ',')
    var <- tok ident
    tok (string "in")
    xs <- access
    return $ VectorT index var xs

conditional :: Lexer Token
conditional = inDirective '?' (IfT <$> condition)

additional :: Lexer Token
additional = inDirective '|' (elseClause <|> ElifT <$> condition)
    where elseClause = try $ do
            tok $ string "else"
            return ElseT

command :: Lexer Token
command = inDirective '%' $ CommandT <$> choice (map tok [extend, include, block, more, super, end])
    where
    extend = string "extends" >> ExtendsT <$> tok strLit
    include = string "include" >> IncludeT <$> tok strLit
    block = string "block" >> BlockT <$> tok ident
    more = string "more" >> MoreT <$> optionMaybe (tok ident)
    super = string "super" >> SuperT <$> optionMaybe (tok ident)
    end = string "end" >> return EndT

function :: Lexer Token
function = inDirective '&' $ CallT <$> tok ident <* space <*> arg `sepBy` space

comment :: Lexer Token
comment = inDirective '#' $ do
    anyChar `manyTill` char '}'
    return CommentT


ident :: Lexer Ident
ident = do 
    hd <- letter
    tl <- many (letter <|> digit)
    return $ pack (hd:tl)

member :: Lexer Member
member = do
    char '.'
    Right <$> ident <|> Left <$> intLit

intLit :: Lexer Integer
intLit = foldl accumInteger 0 <$> many1 digit
    where accumInteger acc x = 10 * acc + (toInteger . digitToInt) x

strLit :: Lexer String
strLit = parserZero --STUB

access :: Lexer Access
access = tok $ do
    root <- ident
    members <- many member
    return $ Access root members

condition :: Lexer Condition
condition = Have <$> access

arg :: Lexer Argument
arg = tok $ choice [Name <$> access, IntLit <$> intLit, StrLit . pack <$> strLit]


------ Helpers ------
type Parser = Parsec [Token] ParserState
type Lexer = Parsec Text ()

openDirective :: Lexer ()
openDirective = void . lookAhead . try $ do
    oneOf "#\\$@?|%&" --TODO MAYBE ! and = directives
    char '{'

inDirective :: Char -> Lexer a -> Lexer a
inDirective sigil p = between (string (sigil:"{")) (tok $ char '}') p

tok :: Lexer a -> Lexer a
tok = between (skipMany space) (skipMany space)

singleton = (:[])
a << b = do { x <- a; b; return x }

satisfy :: (Stream s m t) => (t -> Bool) -> ParsecT s u m t
satisfy f = tokenPrim (const "")
                      (\pos _ _ -> pos)
                      (\x -> if f x then Just x else Nothing)
