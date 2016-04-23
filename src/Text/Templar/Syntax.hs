module Text.Templar.Syntax
    ( Config(..)
    , Name
    , Template(..)
    , Source(..)
    , Chain(..)
    , Field(..)
    ) where

import Data.Text (Text)
import qualified Data.Text as T


data Config = Config
    { cfgStartTag :: Text
    , cfgEndTag :: Text
    -- , cfgTagSyntaxErrorLevel :: Silent | Warning | Error
    , cfgFilter :: (String, Text -> Text)
    -- , cfgOtherFilters :: Name -> (Text -> Text)
    }


type Name = String --FIXME constructor validates string

data Template
    = Literal Text
    | Output Source
    | CondBlock [(Bool, Source, Template)] Template
    | LoopBlock
        { loopName :: Maybe Name
        , loopVar :: Maybe Name
        , loopOver :: Source 
        , loopBody :: Template
        , loopEmpty :: Template
        }
    | Sequence [Template]
    deriving (Show)


data Source = Source Chain [Chain]
    deriving (Show)
data Chain
    = Rooted [Field]
    | Relative [Field]
    | Immediate Text
    deriving (Show)
data Field
    = NameField Name
    | CountField
    deriving (Show)