{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Tiger.Syntax.Parse.Primitives
    ( located
    , space
    , ident
    , keyword
    , int
    , parens
    , braces
    , brackets
    , dquotes
    ) where

--------------------------------------------------------------------------------

import           Control.Monad
import           Data.Char
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import           Text.Megaparsec
import           Text.Megaparsec.Char          hiding (space)
import qualified Text.Megaparsec.Char.Lexer    as L
import           Text.Megaparsec.Error.Builder

import           Data.Symbol
import           Tiger.Reporting.Annotation
import           Tiger.Syntax.Parse

--------------------------------------------------------------------------------

space :: Parser ()
space = L.space space1 empty (L.skipBlockComment "/*" "*/")

--------------------------------------------------------------------------------

-- identifier: [a-z][a-zA-Z0-9_]*
ident :: Parser Symbol
ident = do
    off <- getOffset

    _   <- lookAhead lowerChar
    id' <- takeWhile1P Nothing isIdentChar

    when (id' `S.member` reserved) do
        parseError $ err off (utoks id' <> elabel "identifier")

    pure $! symbol id'


keyword :: T.Text -> Parser ()
keyword kw = void . try $ string kw <* notFollowedBy (satisfy isIdentChar)


reserved :: S.Set T.Text
reserved =
    S.fromList
        [ "let", "in", "end"
        , "if", "then", "else"
        , "while", "for", "to", "do", "break"
        , "array", "of"
        , "nil"
        ]


isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_'

--------------------------------------------------------------------------------

int :: Parser Int
int = L.decimal

--------------------------------------------------------------------------------

parens, braces, brackets, dquotes :: Parser a -> Parser a
parens   = between (char '(')  (char ')')
braces   = between (char '{')  (char '}')
brackets = between (char '[')  (char ']')
dquotes  = between (char '\"') (char '\"')

--------------------------------------------------------------------------------

located :: Parser a -> Parser (Located a)
located p = do
    s <- getOffset
    a <- p
    e <- getOffset
    pure $! at (pos s) (pos e) a
