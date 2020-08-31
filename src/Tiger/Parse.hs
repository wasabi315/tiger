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


chainl1 :: Parser (a -> a -> a) -> Parser a -> Parser a
chainl1 op p = do
    a <- p
    rest a
  where
    rest a = choice
        [ do
            f <- op
            b <- p
            rest (f a b)
        , pure a
        ]


binop :: Parser BinOp -> Parser Expr -> Parser Expr
binop op p = op' `chainl1` p
  where
    op' = do
      A.At r x <- located op
      pure $ \e1 e2 -> A.At r (BinOpExpr x e1 e2)
{-# INLINE binop #-}

-------------------------------------------------------------------------------

pExpr :: Parser Expr
pExpr = pLogOrExpr


pLogOrExpr :: Parser Expr
pLogOrExpr = lexeme $ binop (OrOp <$ symbol "|") pLogAndExpr
{-# INLINE pLogOrExpr #-}


pLogAndExpr :: Parser Expr
pLogAndExpr = lexeme $ binop (AndOp <$ symbol "&") pCmpExpr
{-# INLINE pLogAndExpr #-}


pCmpExpr :: Parser Expr
pCmpExpr = lexeme $ binop op pAddExpr
  where
    op = choice
        [ EqOp  <$ symbol "=="
        , NeqOp <$ symbol "!="
        , GeOp  <$ symbol ">="
        , GtOp  <$ symbol ">"
        , LeOp  <$ symbol "<="
        , LtOp  <$ symbol "<"
        ]
{-# INLINE pCmpExpr #-}


pAddExpr :: Parser Expr
pAddExpr = lexeme $ binop op pMulExpr
  where
    op = (PlusOp <$ symbol "+") <|> (MinusOp <$ symbol "-")
{-# INLINE pAddExpr #-}


pMulExpr :: Parser Expr
pMulExpr = lexeme $ binop op pUnaryExpr
  where
    op = (MulOp <$ symbol "*") <|> (DivOp <$ symbol "/")
{-# INLINE pMulExpr #-}


pUnaryExpr :: Parser Expr
pUnaryExpr = lexeme $ do
    x <- optional $ located (NegOp <$ symbol "-")
    e <- pExprPrimary
    pure $ case x of
        Nothing          -> e
        Just (A.At r op) -> A.At r (UnOpExpr op e)
{-# INLINE pUnaryExpr #-}


pExprPrimary :: Parser Expr
pExprPrimary = lexeme . located $ choice
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
