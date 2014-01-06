module Data.Templar (
      Templar(..)
    , textToTemplar
    ) where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Symbol
import Data.List
import qualified Data.Map as M
import Data.Map (Map)
import Control.Monad


------ Data Structures ------
type Templar a = Map Symbol (TemplarItem a)

data TemplarItem a = Scalar  a
                   | Vector [a]
data Element a = ScalarE Symbol a
               | VectorE Symbol
               | ElemE          a
data Line a = ScalarL a
            | VectorL a
            | ElemL   a
            | ContL   a


------ Instances ------
instance Functor TemplarItem where
    fmap f (Scalar x)  = Scalar (f x)
    fmap f (Vector xs) = Vector (fmap f xs)


------ Parser ------
textToTemplar :: Text -> Either String (Templar Text)
textToTemplar input = do
    ls <- mapM classify . T.lines $ input
    es <- (liftM concat) . mapM mergeScalar . groupBy continued $ ls
    is <- mapM mergeVector . groupBy elements $ es
    return $ M.fromList is
    where
    classify line = do
        when (T.null line) (Left "Bad Templar Data: empty line.")
        let Just (sigil, content) = T.uncons line
        case sigil of
            '$' -> Right $ ScalarL (T.tail line)
            '@' -> Right $ VectorL (T.tail line)
            ',' -> Right $ ElemL   (T.tail line)
            ':' -> Right $ ContL   (T.tail line)
            _   -> Left  "Bad Templar Data: lines must begin with a sigil, one of '$', '@', ',', ':'."
    
    continued _ (ContL _) = True
    continued _ _         = False
    
    mergeScalar (ScalarL name: xs) = Right [ScalarE (toSymbol name)        $ unLines             xs]
    mergeScalar (VectorL name: xs) = Right [VectorE (toSymbol name), ElemE $ unLines             xs]
    mergeScalar (ElemL   x   : xs) = Right [                         ElemE $ unLines $ ElemL x : xs]
    mergeScalar (ContL   _   : _ ) = Left  "Bad Templar Data: continuation lines must follow some other sort of line."
    
    elements (VectorE _) (ElemE _) = True
    elements (ElemE   _) (ElemE _) = True
    elements _           _         = False
    
    mergeVector [ScalarE name  x ] = Right (name, Scalar x)
    mergeVector (VectorE name: xs) = Right (name, Vector $ map unElement xs)
    mergeVector (ElemE   x   : _ ) = Left  "Bad Templar Data: vector elements must occur after a vector begin or other vector element."

    toSymbol = intern . T.unpack
    unLines = T.concat . map unLine

------ Simple Helpers ------
unLine (ScalarL x) = x
unLine (VectorL x) = x
unLine (ElemL x)   = x
unLine (ContL x)   = x
unElement (ElemE x) = x
