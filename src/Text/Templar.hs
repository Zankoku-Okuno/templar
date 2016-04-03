{-#LANGUAGE OverloadedStrings,
            PatternSynonyms, RecordWildCards, LambdaCase,
            ExistentialQuantification #-}
module Text.Templar where

import Text.Templar.Syntax
import Text.Templar.Knightly
import Text.Templar.Render

import Data.Text (Text)
import qualified Data.Text as T










    

render :: Knightly k => Config -> String -> k -> IO Text
render config template knight = do
    let parsed = parseTemplate config template
    rendered <- runRender config knight (renderTemplate parsed)
    return rendered
    where
    parseTemplate = undefined



-- parse string literal
-- parse name
-- parse chain (above, names separated by dots)
-- parse tag (above, but parse first char for operator, spaces for function args, comment)