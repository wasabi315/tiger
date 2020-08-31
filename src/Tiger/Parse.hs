{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Tiger.Parse
    ( Parser
    , ident
    , integer
    , float
    , string
    , character
    , keyword
    , parens
    , braces
    , semi
    , colon
    , pExpr
    , module Text.Megaparsec
    , module Text.Megaparsec.Char
    ) where

import           Data.Char
import           Data.Text                  (Text)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char       hiding (string)
import qualified Text.Megaparsec.Char.Lexer as L

import           Tiger.AST
import qualified Tiger.Reporting.Annotation as A

-------------------------------------------------------------------------------

type Parser = Parsec Void Text

-------------------------------------------------------------------------------
-- Lexer

sc :: Parser ()
sc = L.space space1 empty (L.skipBlockComment "/*" "*/")
{-# INLINE sc #-}

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
{-# INLINE lexeme #-}

symbol :: Text -> Parser Text
symbol = L.symbol sc
{-# INLINE symbol #-}

isValidIdentChar :: Char -> Bool
isValidIdentChar c = isAlphaNum c || c == '_'
{-# INLINE isValidIdentChar #-}

ident :: Parser Text
ident = do
    _ <- lookAhead letterChar
    takeWhile1P Nothing isValidIdentChar
{-# INLINE ident #-}

integer :: Parser Int
integer = L.decimal
{-# INLINE integer #-}

float :: Parser Double
float = L.float
{-# INLINE float #-}

string :: Parser Text
string = between (char '\"') (char '\"') (takeWhile1P Nothing (/= '"'))
{-# INLINE string #-}

character :: Parser Char
character = between (char '\'') (char '\'') anySingle
{-# INLINE character #-}

keyword :: Text -> Parser Text
keyword k = chunk k <* notFollowedBy (satisfy isValidIdentChar)
{-# INLINE keyword #-}

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
{-# INLINE parens #-}

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")
{-# INLINE braces #-}

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")
{-# INLINE brackets #-}

semi :: Parser Text
semi = symbol ";"
{-# INLINE semi #-}

colon :: Parser Text
colon = symbol ":"
{-# INLINE colon #-}

comma :: Parser Text
comma = symbol ","
{-# INLINE comma #-}

-------------------------------------------------------------------------------

located :: Parser a -> Parser (A.Located a)
located p = do
    s <- getSourcePos
    a <- p
    e <- getSourcePos
    pure $! A.located s e a
{-# INLINE located #-}

-------------------------------------------------------------------------------

pExpr :: Parser Expr
pExpr = lexeme . located $ choice
    [ try pNilExpr
    , pIntExpr
    , pStrExpr
    , try pCallExpr
    , pVarExpr
    ]


pVarExpr :: Parser Expr_
pVarExpr = VarExpr <$> pVar
{-# INLINE pVarExpr #-}

pVar :: Parser Var
pVar = located $ do
    i <- ident
    pVarHelper (SimpleVar i)
  where
    pVarHelper v = choice
        [ do
            char '.'
            f <- located ident
            pVarHelper (FieldVar v f)
        , brackets $ do
            e <- pExpr
            pVarHelper (SubscriptVar v e)
        , pure v
        ]


pNilExpr :: Parser Expr_
pNilExpr = NilExpr <$ keyword "nil"
{-# INLINE pNilExpr #-}


pIntExpr :: Parser Expr_
pIntExpr = IntExpr <$> integer
{-# INLINE pIntExpr #-}


pStrExpr :: Parser Expr_
pStrExpr = StrExpr <$> string
{-# INLINE pStrExpr #-}


pCallExpr :: Parser Expr_
pCallExpr = do
    i <- ident
    args <- parens (pExpr `sepBy` comma)
    pure (CallExpr i args)
{-# INLINE pCallExpr #-}
