{-#LANGUAGE OverloadedStrings,
            ViewPatterns, RecordWildCards,
            ExistentialQuantification,
            TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}
module Text.Templar.Knightly where

import Text.Templar.Syntax (Name)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Functor
import Control.Applicative
import Control.Monad.IO.Class


data EscapeStatus = AlreadyEscaped | NeedsEscaping
data Extract r
    = Good r
    | Query (IO (Extract r))
    | Error String

data Knight = forall a. Knightly a => Knight a
class Knightly a where
    output :: a -> Extract (Text, EscapeStatus)
    truthy :: a -> Extract Bool
    access :: a -> Name -> Extract Knight
    loop :: a -> Extract [Knight]
    count :: a -> Extract Knight
    call :: a -> [Knight] -> Extract Knight

    output _ = Error "not renderable"
    truthy _ = Good True
    access _ = Good . Knight . NoSuchField
    loop _ = Error "not a collection"
    count self = case loop self of
        Good xs -> Good . Knight $ getInt xs
        Query action -> Query $ (Knight . getInt <$>) <$> action
        Error err -> Error err
        where
        getInt :: [a] -> Integer
        getInt xs = fromIntegral (length xs) :: Integer
    call _ _ = Error "not callable"


newtype NoSuchField = NoSuchField Name
instance Knightly NoSuchField where
    output (NoSuchField name) = Error $ "no such field: `" ++ name ++ "`"
    truthy _ = Good False
    access self _ = Good . Knight $ self
    loop (NoSuchField name) = Error $ "no such field: `" ++ name ++ "`"
    count (NoSuchField name) = Error $ "no such field: `" ++ name ++ "`"
    call (NoSuchField name) _ = Error $ "no such field: `" ++ name ++ "`"


instance Functor Extract where
    fmap f (Good a) = Good (f a)
    fmap f (Query action) = Query ((f <$>) <$> action)
    fmap f (Error msg) = Error msg
instance Applicative Extract where
    pure = Good
    (Good f) <*> (Good a) = Good $ f a
    (Good f) <*> (Query action) = Query ((f <$>) <$> action)
    (Query action) <*> a = Query ((<*> a) <$> action)
    (Error msg) <*> _ = Error msg
    _ <*> (Error msg) = Error msg    
instance Alternative Extract where
    empty = Error "unknown error"
    (Error _) <|> right = right
    left <|> _ = left
instance Monad Extract where
    return = pure
    (Good x) >>= k = k x
    (Query action) >>= k = Query $ (k =<<) <$> action
    (Error msg) >>= k = Error msg


instance Knightly Knight where
    output (Knight it) = output it
    truthy (Knight it) = truthy it
    access (Knight it) = access it
    loop (Knight it) = loop it
    count (Knight it) = count it
    call (Knight it) = call it

instance Knightly Bool where
    truthy b = Good b

instance Knightly Integer where
    output i = Good $ (T.pack . show $ i, NeedsEscaping)
    truthy 0 = Good False
    truthy _ = Good True

instance Knightly String where
    output str = Good (T.pack str, NeedsEscaping)
    truthy "" = Good False
    truthy _ = Good True

-- TODO the other (lazy/strict) text type
instance Knightly Text where
    output str = Good (str, NeedsEscaping)
    truthy "" = Good False
    truthy _ = Good True

instance Knightly a => Knightly (Maybe a) where
    output Nothing = Good ("", NeedsEscaping)
    output (Just x) = output x
    truthy Nothing = Good False
    truthy (Just _) = Good True
    loop Nothing = Good []
    loop (Just x) = Good [Knight x]

instance Knightly a => Knightly [a] where
    truthy [] = Good False
    truthy _ = Good True
    access xs (discrim -> Right i) = Good . Knight $ xs !! i
    access xs (discrim -> Left name) = Good . Knight $ NoSuchField name
    loop = Good . map Knight

instance (Knightly a, Knightly b) => Knightly (a, b) where
    truthy _ = Good True
    access (a, _) "0" = Good . Knight $ a
    access (_, b) "1" = Good . Knight $ b
    access _ name = Good . Knight $ NoSuchField name
    loop (a, b) = Good [Knight a, Knight b]
--TODO more tuples

newtype KeyVal k v = KeyVal (k, v)
instance (Knightly a, Knightly b) => Knightly (KeyVal a b) where
    output (KeyVal x) = output x
    truthy (KeyVal x) = truthy x
    access (KeyVal (k, _)) "key" = Good . Knight $ k
    access (KeyVal (_, v)) "val" = Good . Knight $ v
    access (KeyVal x) name = access x name
    loop (KeyVal x) = loop x
    count (KeyVal x) = count x
    call (KeyVal x) = call x

instance Knightly v => Knightly [(Name, v)] where
    truthy [] = Good False
    truthy _ = Good True
    access xs name = case lookup name xs of
        Nothing -> Good . Knight $ NoSuchField name
        Just val -> Good . Knight $ val
    loop xs = Good . map Knight $ map KeyVal xs

-- TODO the other (lazy/strict) map type
instance Knightly a => Knightly (Map Name a) where
    truthy xs = Good $ not (Map.null xs)
    access self name = case Map.lookup name self of
        Nothing -> Good . Knight $ NoSuchField name
        Just it -> Good . Knight $ it
    loop xs = loop $ Map.assocs xs
    count xs = Good . Knight $ (fromIntegral $ Map.size xs :: Integer)

instance Knightly a => Knightly ([Knight] -> Extract a) where
    truthy _ = Error "not testable"
    call f xs = Knight <$> f xs


data CmdlineQuery a = CmdlineQuery
    { cmdlineAsk :: String -> IO String
    , cmdlineQuestion :: String
    , cmdlineDefault :: Maybe a
    , cmdlineShowDefault :: Maybe String
    , cmdlineParser :: String -> Maybe a
    }
runCmdlineQuery :: CmdlineQuery a -> Extract a
runCmdlineQuery (CmdlineQuery {..}) = Query $ loop
    where
    q = cmdlineQuestion
        ++ (maybe "" (\str -> " [" ++ str ++ "]: ") cmdlineShowDefault)
    loop = do
        a <- cmdlineAsk q
        case (a, cmdlineDefault, cmdlineParser a) of
            ("", Just def, _) -> pure . Good $ def
            (_, _, Just it) -> pure . Good $ it
            (_, _, Nothing) -> loop

instance Knightly a => Knightly (CmdlineQuery a) where
    output q = runCmdlineQuery q >>= output
    truthy q = runCmdlineQuery q >>= truthy
    access q name = runCmdlineQuery q >>= flip access name 
    loop q = runCmdlineQuery q >>= loop
    count q = runCmdlineQuery q >>= count
    call q args = runCmdlineQuery q >>= flip call args

--TODO json, yaml, xml, ini, csv


discrim :: Name -> Either Name Int
discrim str | check str = Right $ foldl go 0 str
            | otherwise = Left str
    where
    check = all (`elem` ['0'..'9'])
    go acc c = fromEnum c - fromEnum '0' + acc * 10