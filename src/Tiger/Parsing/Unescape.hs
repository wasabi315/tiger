{-# OPTIONS_GHC -Wall -Wno-unused-do-bind #-}
{-# LANGUAGE BlockArguments #-}

module Tiger.Parsing.Unescape
    ( strLit
    ) where

import           Control.Monad
import           Data.Char
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import           Text.Megaparsec.Error.Builder

import           Tiger.Parsing.Primitives      (Parser)

--------------------------------------------------------------------------------

strLit :: Parser Text
strLit = do
    str <- between (char '"') (char '"') do
        optional $ try skipFmtChar
        many (strChar <* optional (try skipFmtChar))
    pure $! T.pack str
{-# INLINE strLit #-}


skipFmtChar :: Parser ()
skipFmtChar = void $ between
    (char '\\')
    (char '\\')
    (takeWhileP Nothing (\c -> c `elem` (" \t\n\f" :: String)))


strChar :: Parser Char
strChar = strLetter <|> strEscape
{-# INLINE strChar #-}


strLetter :: Parser Char
strLetter = satisfy (\c -> c `notElem` ("\"\\\026" :: String))
{-# INLINE strLetter #-}


strEscape :: Parser Char
strEscape = do
    char '\\'
    charEsc <|> charCtrl <|> charNum


charEsc :: Parser Char
charEsc = choice
    [ '\n' <$ char 'n'
    , '\t' <$ char 't'
    , '"'  <$ char '"'
    , '\\' <$ char '\\'
    ]


charCtrl :: Parser Char
charCtrl = do
    char '^'
    code <- upperChar
    pure $! chr (ord code - ord 'A' + 1)


charNum :: Parser Char
charNum = do
    o <- getOffset
    d <- L.decimal
    if d > 0 && d < 128
        then pure $! chr d
        else parseError $ err o (ulabel "invalid ascii code")
