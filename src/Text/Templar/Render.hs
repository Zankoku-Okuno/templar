{-#LANGUAGE OverloadedStrings,
            PatternSynonyms, RecordWildCards, LambdaCase #-}
module Text.Templar.Render where

import Text.Templar.Syntax
import Text.Templar.Knightly
import Text.Templar.Source

import Data.Text (Text)
import qualified Data.Text as T
import Control.Applicative
import Control.Monad.Reader


type Render a = ReaderT Context IO a
data Context = Context
    { ctxConfig :: Config
    , ctxRoot :: Knight
    , ctxCurrent :: Knight
    }
runRender :: Knightly k => Config -> k -> Render a -> IO a
runRender config knight action = runReaderT action $
    Context { ctxConfig = config
            , ctxRoot = Knight knight
            , ctxCurrent = Knight knight
            }


data LoopContext = LoopContext
    { parent :: Knight
    , theLoop :: Maybe (Name, LoopIndex) 
    , loopElem :: Maybe (Name, Knight)
    }
data LoopIndex = LoopIndex Integer Integer -- current index, last index
mkLoopContexts :: (Maybe Name, Maybe Name) -> Knight -> [Knight] -> [LoopContext]
mkLoopContexts (loopName, elemName) parent elems = zipWith go [1..] elems
    where
    go i elem = LoopContext
        { parent = parent
        , theLoop = mkTheLoop <$> loopName
        , loopElem = mkTheElem <$> elemName
        }
        where
        mkTheLoop name = (name, LoopIndex i len)
        mkTheElem name = (name, elem)
    len = fromIntegral $ length elems
instance Knightly LoopContext where
    access (LoopContext {..}) name = fromElemName <|> fromLoopName <|> fromParent
        where
        fromElemName = case loopElem of
            (Just (elemName, it)) | name == elemName -> Good it
            _ -> Error (error precondition)
        fromLoopName = case theLoop of
            (Just (loopName, it)) | name == loopName -> Good (Knight it)
            _ -> Error (error precondition)
        fromParent = access parent name
        precondition = "INTERNAL ERROR (please report): name should have been looked up in LoopContext.parent"
instance Knightly LoopIndex where
    output (LoopIndex i _) = Good $ (T.pack $ show i, NeedsEscaping)
    count (LoopIndex _ e) = Good . Knight $ e

renderTemplate :: Template -> Render Text
renderTemplate (Literal str) = pure str
renderTemplate (Output source) = do
    Context {..} <- ask
    knight <- extract $ navigate (ctxRoot, ctxCurrent) source
    (str, escape) <- extract $ output knight
    pure $ case escape of
        NeedsEscaping -> (cfgFilter ctxConfig) str
        AlreadyEscaped -> str
renderTemplate (CondBlock [] elseTemplate) = renderTemplate elseTemplate
renderTemplate (CondBlock ((expected, predicate, thenTemplate):rest) elseTemplate) = do
    Context {..} <- ask
    knight <- extract $ navigate (ctxRoot, ctxCurrent) predicate
    truthiness <- extract $ truthy knight
    renderTemplate $ if truthiness == expected
        then thenTemplate
        else CondBlock rest elseTemplate
renderTemplate (LoopBlock {..}) = do
    Context {..} <- ask
    knight <- extract $ navigate (ctxRoot, ctxCurrent) loopOver
    elements <- extract $ loop knight
    let loopCtxs = mkLoopContexts (loopName, loopVar) ctxCurrent elements
    T.concat <$> mapM renderLoopBody loopCtxs
    where
    renderLoopBody loopCtx =
        let enterLoopContext ctx = ctx { ctxCurrent = Knight loopCtx }
        in local enterLoopContext $ renderTemplate loopBody
renderTemplate (Sequence xs) =
    T.concat <$> mapM renderTemplate xs


extract :: Extract r -> Render r
extract (Good it) = pure it
extract (Query action) = liftIO action >>= extract
extract (Error msg) = error msg -- FIXME better error reporting