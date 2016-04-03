{-#LANGUAGE OverloadedStrings,
            ExistentialQuantification,
            TypeSynonymInstances, FlexibleInstances #-}
module Text.Templar.Knightly where

import Text.Templar.Syntax (Name)

import Data.Text (Text)
import qualified Data.Text as T
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
    truthy _ = Error "not testable"
    access _ name = Error $ "no such field (" ++ show name ++ ")"
    loop _ = Error "not a collection"
    count _ = Error "not a collection"
    call _ _ = Error "not callable"


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
instance Knightly String where
    output str = Good (T.pack str, NeedsEscaping)
    truthy "" = Good False
    truthy _ = Good True
instance Knightly Text where
    output str = Good (str, NeedsEscaping)
    truthy "" = Good False
    truthy _ = Good True
instance Knightly Integer where
    output i = Good $ (T.pack . show $ i, NeedsEscaping)
    truthy 0 = Good False
    truthy _ = Good True
