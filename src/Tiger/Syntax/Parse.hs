module Tiger.Syntax.Parse
    ( Parser
    , Tiger.Syntax.Parse.parse
    , located
    ) where

--------------------------------------------------------------------------------

import qualified Data.Text                  as T
import           Data.Void
import           Text.Megaparsec

import           Tiger.Reporting.Annotation

--------------------------------------------------------------------------------

type Parser = Parsec Void T.Text

--------------------------------------------------------------------------------

parse
    :: Parser a
    -> String
    -> T.Text
    -> Either (ParseErrorBundle T.Text Void) a
parse = Text.Megaparsec.parse

--------------------------------------------------------------------------------

located :: Parser a -> Parser (Located a)
located p = do
    s <- getOffset
    a <- p
    e <- getOffset
    pure $! at (pos s) (pos e) a
