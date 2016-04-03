module Text.Templar.Syntax where

import Data.Text (Text)
import qualified Data.Text as T


data Config = Config
    { cfgStartTag :: Text
    , cfgEndTag :: Text
    -- , cfgTagSyntaxErrorLevel :: Silent | Warning | Error
    , cfgFilter :: Text -> Text
    -- , cfgOtherFilters :: Name -> (String -> String)
    }


type Name = String --FIXME constructor validates string

data Template
    = Literal Text
    | Output Source
    | CondBlock [(Source, Template)] Template
    | LoopBlock
        { loopName :: Maybe Name
        , loopVar :: Maybe Name
        , loopOver :: Source 
        , loopBody :: Template
        }
    | Sequence [Template]


data Source = Source Chain [Chain]
data Chain
    = Rooted [Field]
    | Relative [Field]
    | Immediate Text
data Field
    = NameField Name
    | CountField