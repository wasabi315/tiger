{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Tiger.Parse
    ( module Tiger.Parse
    , module Text.Megaparsec
    , module Text.Megaparsec.Char
    ) where

import           Control.Monad.Combinators.Expr
import           Data.Char
import           Data.Text                      (Text)
import qualified Data.Text                      as T
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
    c  <- letterChar
    cs <- takeWhile1P Nothing isValidIdentChar
    pure $! c `T.cons` cs
{-# INLINE ident #-}


integer :: Parser Int
integer = L.decimal
{-# INLINE integer #-}


string :: Parser Text
string = between (char '\"') (char '\"') (takeWhile1P Nothing (/= '"'))
{-# INLINE string #-}


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


prefix :: Text -> (LcExpr -> Expr) -> Operator Parser LcExpr
prefix op f = Prefix $ do
    A.At r _ <- located (symbol op)
    pure $ \e -> A.At r (f e)
{-# INLINE prefix #-}


binaryL :: Text -> (LcExpr -> LcExpr -> Expr) -> Operator Parser LcExpr
binaryL op f = InfixL $ do
    A.At r _ <- located (symbol op)
    pure $ \e1 e2 -> A.At r (f e1 e2)
{-# INLINE binaryL #-}


binaryN :: Text -> (LcExpr -> LcExpr -> Expr) -> Operator Parser LcExpr
binaryN op f = InfixN $ do
    A.At r _ <- located (symbol op)
    pure $ \e1 e2 -> A.At r (f e1 e2)
{-# INLINE binaryN #-}

-------------------------------------------------------------------------------

pLcExpr :: Parser LcExpr
pLcExpr = choice
    [ pValue
    ]


pLcTypeId :: Parser LcType
pLcTypeId = lexeme $ do
    A.At r i <- located ident
    pure $ A.At r (NameTy i)


pVar :: Parser Expr
pVar = fmap VarExpr . located $ do
    i <- ident
    rest (NameVar i)
    where
        rest v = choice
            [ do
                _ <- char '.'
                f <- located ident
                rest (FieldVar v f)
            , brackets $ do
                e <- pLcExpr
                rest (IdxedVar v e)
            , pure v
            ]


pValue :: Parser LcExpr
pValue = lexeme $ makeExprParser pPrimary table


table :: [[Operator Parser LcExpr]]
table =
    [ [ prefix  "-"  (UopExpr NegOp)
      ]
    , [ binaryL "*"  (BopExpr MulOp)
      , binaryL "/"  (BopExpr DivOp)
      ]
    , [ binaryL "+"  (BopExpr AddOp)
      , binaryL "-"  (BopExpr SubOp)
      ]
    , [ binaryN "==" (BopExpr EqOp)
      , binaryN "!=" (BopExpr NeqOp)
      , binaryN ">=" (BopExpr GeOp)
      , binaryN ">"  (BopExpr GtOp)
      , binaryN "<=" (BopExpr LeOp)
      , binaryN "<"  (BopExpr LtOp)
      ]
    , [ binaryL "&"  (BopExpr AndOp)
      ]
    , [ binaryL "|"  (BopExpr OrOp)
      ]
    ]


-- FIXME: Avoid try
pPrimary :: Parser LcExpr
pPrimary =
    (lexeme . located $ choice
        [ pUnit_
        , try pNil_
        , pInteger_
        , pString_
        , try pRecord_
        , try pArray_
        , try pCall_
        , pVar
        ])
    <|> parens pLcExpr


pCall_ :: Parser Expr
pCall_ =  do
    i <- ident
    args <- parens (pLcExpr `sepBy` comma)
    pure (CallExpr i args)


pUnit_ :: Parser Expr
pUnit_ = UnitExpr <$ chunk "()"


pNil_ :: Parser Expr
pNil_ = NilExpr <$ keyword "nil"


pInteger_ :: Parser Expr
pInteger_ = IntExpr <$> integer


pString_ :: Parser Expr
pString_ = StrExpr <$> string


pRecord_ :: Parser Expr
pRecord_ = do
    ty <- pLcTypeId
    fs <- braces $ kvPair `sepBy` comma
    pure (RecordExpr fs ty)
    where
        kvPair = do
            f <- lexeme (located ident)
            _ <- symbol "="
            e <- pLcExpr
            pure (f, e)


pArray_ :: Parser Expr
pArray_ = do
    ty <- pLcTypeId
    e1 <- brackets pLcExpr
    _  <- lexeme $ keyword "of"
    e2 <- pLcExpr
    pure (ArrayExpr ty e1 e2)
