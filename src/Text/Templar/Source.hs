module Text.Templar.Source where

import Text.Templar.Syntax
import Text.Templar.Knightly


navigate :: (Knight, Knight) -> Source -> Extract Knight
navigate ctx (Source chain []) = navChain ctx chain
navigate ctx (Source f args) = do
    f' <- call <$> navChain ctx f
    args' <- traverse (navChain ctx) args
    f' args'

navChain :: (Knight, Knight) -> Chain -> Extract Knight
navChain (root, _) (Rooted fields) = navFields root fields
navChain (_, local) (Relative fields) = navFields local fields
navChain _ (Immediate str) = Good . Knight $ str

navFields :: Knight -> [Field] -> Extract Knight
navFields knight [] = Good knight
navFields knight (field:fields) = do
    next <- case field of
        NameField name -> knight `access` name
        CountField -> count knight
    navFields next fields
