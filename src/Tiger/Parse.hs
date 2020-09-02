{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Tiger.Parse
    ( Parser
    , pExpr
    , pRecord_
    , pArray_
    , module Text.Megaparsec
    , module Text.Megaparsec.Char
    ) where

import           Control.Monad.Combinators.Expr
import           Data.Char
import           Data.Text                      (Text)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char           hiding (string)
import qualified Text.Megaparsec.Char.Lexer     as L

import           Tiger.AST
import qualified Tiger.Reporting.Annotation     as A

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


prefix :: Text -> (Expr -> Expr_) -> Operator Parser Expr
prefix op f = Prefix $ do
    A.At r _ <- located (symbol op)
    pure $ \e -> A.At r (f e)
{-# INLINE prefix #-}


binaryL :: Text -> (Expr -> Expr -> Expr_) -> Operator Parser Expr
binaryL op f = InfixL $ do
    A.At r _ <- located (symbol op)
    pure $ \e1 e2 -> A.At r (f e1 e2)
{-# INLINE binaryL #-}


binaryN :: Text -> (Expr -> Expr -> Expr_) -> Operator Parser Expr
binaryN op f = InfixN $ do
    A.At r _ <- located (symbol op)
    pure $ \e1 e2 -> A.At r (f e1 e2)
{-# INLINE binaryN #-}

-------------------------------------------------------------------------------

pExpr :: Parser Expr
pExpr = choice
    [ pValue
    ]


pTypeId :: Parser Type
pTypeId = lexeme $ do
    A.At r i <- located ident
    pure $ A.At r (NameTy i)


pLvar_ :: Parser Expr_
pLvar_ = fmap VarExpr . located $ do
    i <- ident
    rest (SimpleVar i)
  where
    rest v = choice
        [ do
            _ <- char '.'
            f <- located ident
            rest (FieldVar v f)
        , brackets $ do
            e <- pExpr
            rest (SubscriptVar v e)
        , pure v
        ]


pValue :: Parser Expr
pValue = lexeme $ makeExprParser pPrimary table


table :: [[Operator Parser Expr]]
table =
    [ [ prefix  "-"  (UnOpExpr  NegOp) ]
    , [ binaryL "*"  (BinOpExpr MulOp)
      , binaryL "/"  (BinOpExpr DivOp)
      ]
    , [ binaryL "+"  (BinOpExpr PlusOp)
      , binaryL "-"  (BinOpExpr MinusOp)
      ]
    , [ binaryN "==" (BinOpExpr EqOp)
      , binaryN "!=" (BinOpExpr NeqOp)
      , binaryN ">=" (BinOpExpr GeOp)
      , binaryN ">"  (BinOpExpr GtOp)
      , binaryN "<=" (BinOpExpr LeOp)
      , binaryN "<"  (BinOpExpr LtOp)
      ]
    , [ binaryL "&"  (BinOpExpr AndOp) ]
    , [ binaryL "|"  (BinOpExpr OrOp) ]
    ]


-- FIXME: Avoid try
pPrimary :: Parser Expr
pPrimary =
    (lexeme . located $ choice
        [ pNoValue_
        , try pNil_
        , pInteger_
        , pString_
        , try pRecord_
        , try pArray_
        , try pCall_
        , pLvar_
        ])
    <|> parens pExpr


pCall_ :: Parser Expr_
pCall_ =  do
    i <- ident
    args <- parens (pExpr `sepBy` comma)
    pure (CallExpr i args)


pNoValue_ :: Parser Expr_
pNoValue_ = NoValueExpr <$ chunk "()"


pNil_ :: Parser Expr_
pNil_ = NilExpr <$ keyword "nil"


pInteger_ :: Parser Expr_
pInteger_ = IntExpr <$> integer


pString_ :: Parser Expr_
pString_ = StrExpr <$> string


pRecord_ :: Parser Expr_
pRecord_ = do
    ty <- pTypeId
    fs <- braces $ kvPair `sepBy` comma
    pure (RecordExpr fs ty)
  where
    kvPair = do
        f <- lexeme (located ident)
        _ <- symbol "="
        e <- pExpr
        pure (f, e)


pArray_ :: Parser Expr_
pArray_ = do
    ty <- pTypeId
    e1 <- brackets pExpr
    _  <- lexeme $ keyword "of"
    e2 <- pExpr
    pure (ArrayExpr ty e1 e2)
