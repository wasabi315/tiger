{-# OPTIONS_GHC -Wall #-}

module Tiger.Parsing.Primitives
    ( Parser
    , parseFromText
    , located
    ) where

import           Control.Exception          (SomeException (..))
import           Data.Text                  (Text)
import           Data.Void
import           Text.Megaparsec
import qualified Tiger.Reporting.Annotation as A

-------------------------------------------------------------------------------

type Parser = Parsec Void Text

-------------------------------------------------------------------------------

parseFromText :: Parser a -> Text -> Either SomeException a
parseFromText p input = case Text.Megaparsec.parse p "" input of
    Left  perr -> Left (SomeException perr)
    Right expr -> Right expr

-------------------------------------------------------------------------------

located :: Parser a -> Parser (A.Located a)
located p = do
    s <- getOffset
    a <- p
    e <- getOffset
    pure $! A.located s e a
{-# INLINE located #-}
