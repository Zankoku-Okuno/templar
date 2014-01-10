module Templar.Data (
    -- * TemplarData Type
      TemplarData
    , Item(..)
    -- * Conversion
    , textToTemplarData
    -- * Query
    , elem
    , lookup
    , findWithDefault
    , elemSym
    , lookupSym
    , findWithDefaultSym
    ) where

import Prelude hiding (elem, lookup)

import qualified Data.Text as T
import Data.Text (Text)
import Data.Symbol
import Data.List (groupBy, intercalate)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Monoid
import Data.Foldable (Foldable, foldMap)
import Data.Traversable (Traversable, traverse)
import Control.Applicative
import Control.Monad


------ Data Structures ------
newtype TemplarData a = TemplarData { unTemplar :: Map Symbol (Item a) }

data Item a = Scalar  a
            | Vector [a]
data Element a = ScalarE Symbol a
               | VectorE Symbol
               | ElemE          a
data Line a = ScalarL a
            | VectorL a
            | ElemL   a
            | ContL   a
            | BlankL


------ Instances ------
instance Show a => Show (TemplarData a) where
    show xs = "{" ++ intercalate ", " (showElem <$> toList xs) ++ "}"
        where
        showElem (k, Scalar v) = show (unintern k) ++ "=" ++ show v
        showElem (k, Vector vs) = show (unintern k) ++ "=" ++ "[" ++ intercalate ", " (map show vs) ++ "]"

instance Functor TemplarData where
    fmap f = fromList . map (\(k, v) -> (k, f <$> v)) . toList
instance Functor Item where
    fmap f (Scalar x)  = Scalar (f x)
    fmap f (Vector xs) = Vector (fmap f xs)

instance Foldable TemplarData where
    foldMap f = foldMap (foldMap f . snd) . toList
instance Foldable Item where
    foldMap f (Scalar x) = f x
    foldMap f (Vector xs) = foldMap f xs

instance Traversable TemplarData where
    traverse f = (fromList <$>) . traverse (\(k, v) -> (,) k <$> f `traverse` v) . toList
instance Traversable Item where
    traverse f (Scalar x)  = Scalar <$> f x
    traverse f (Vector xs) = Vector <$> f `traverse` xs

instance Monoid (TemplarData a) where
    mempty = TemplarData M.empty
    mappend a b = TemplarData $ unTemplar b `M.union` unTemplar a


------ Data Access ------
elem :: String -> TemplarData a -> Bool
elem = elemSym . intern
lookup :: TemplarData a -> String -> Maybe (Item a)
lookup xs = lookupSym xs . intern
findWithDefault :: (Item a) -> TemplarData a  -> String -> Item a
findWithDefault v xs = findWithDefaultSym v xs . intern

elemSym :: Symbol -> TemplarData a -> Bool
elemSym k xs = M.member k (unTemplar xs)
lookupSym :: TemplarData a -> Symbol -> Maybe (Item a)
lookupSym xs k = M.lookup k (unTemplar xs)
findWithDefaultSym :: (Item a) -> TemplarData a -> Symbol -> Item a
findWithDefaultSym v xs k = M.findWithDefault v k (unTemplar xs)


------ Conversion ------
textToTemplarData :: Text -> Either String (TemplarData Text)
textToTemplarData input = do
    ls <- liftM removeBlanks . mapM classify . T.lines $ input
    es <- (liftM concat) . mapM mergeScalar . groupBy continued $ ls
    is <- mapM mergeVector . groupBy elements $ es
    return . TemplarData . M.fromList $ is
    where
    classify line = do
        when (T.null line) (Left "Bad TemplarData Data: empty line.")
        let Just (sigil, content) = T.uncons line
        case sigil of
            '$' -> Right $ ScalarL (T.tail line)
            '@' -> Right $ VectorL (T.tail line)
            ',' -> Right $ ElemL   (T.tail line)
            ':' -> Right $ ContL   (T.tail line)
            '#' -> Right $ BlankL
            _   -> Left  "Bad TemplarData Data: lines must begin with a sigil, one of '$', '@', ',', ':', '#'."
    
    continued _ (ContL _) = True
    continued _ _         = False
    
    mergeScalar (ScalarL name: xs) = Right [ScalarE (toSymbol name)        $ unLines             xs]
    mergeScalar (VectorL name: xs) = Right [VectorE (toSymbol name), ElemE $ unLines             xs]
    mergeScalar (ElemL   x   : xs) = Right [                         ElemE $ unLines $ ElemL x : xs]
    mergeScalar (ContL   _   : _ ) = Left  "Bad TemplarData Data: continuation lines must follow some other sort of line."
    
    elements (VectorE _) (ElemE _) = True
    elements (ElemE   _) (ElemE _) = True
    elements _           _         = False
    
    mergeVector [ScalarE name  x ] = Right (name, Scalar x)
    mergeVector (VectorE name: xs) = Right (name, Vector $ map unElement xs)
    mergeVector (ElemE   x   : _ ) = Left  "Bad TemplarData Data: vector elements must occur after a vector begin or other vector element."

    toSymbol = intern . T.unpack
    unLines = T.intercalate (T.pack "\n") . map unLine
    removeBlanks = filter $ \x -> case x of { BlankL -> False; _ -> True }

------ Simple Helpers ------
unLine (ScalarL x) = x
unLine (VectorL x) = x
unLine (ElemL x)   = x
unLine (ContL x)   = x
unElement (ElemE x) = x

toList = M.toList . unTemplar
fromList = TemplarData . M.fromList
