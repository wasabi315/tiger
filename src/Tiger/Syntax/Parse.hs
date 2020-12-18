module Tiger.Syntax.Parse
    ( Parser
    , Tiger.Syntax.Parse.parse
    ) where

--------------------------------------------------------------------------------

import qualified Data.Text       as T
import           Data.Void
import           Text.Megaparsec

--------------------------------------------------------------------------------

type Parser = Parsec Void T.Text

--------------------------------------------------------------------------------

parse
    :: Parser a
    -> String
    -> T.Text
    -> Either (ParseErrorBundle T.Text Void) a
parse = Text.Megaparsec.parse
