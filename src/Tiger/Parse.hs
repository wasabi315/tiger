{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Tiger.Parse
    ( module Tiger.Parse
    , module Text.Megaparsec
    , module Text.Megaparsec.Char
    ) where

import           Control.Monad.Combinators.Expr
import           Data.Char
import           Data.Set                       (Set)
import qualified Data.Set                       as S
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char           hiding (string)
import qualified Text.Megaparsec.Char.Lexer     as L
import           Text.Megaparsec.Error.Builder

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
    o  <- getOffset
    c  <- letterChar
    cs <- takeWhileP Nothing isValidIdentChar
    let !i = c `T.cons` cs
    if i `S.member` reserved
        then parseError $ err o (utoks i <> elabel "identifier")
        else pure i


reserved :: Set Symbol
reserved = S.fromList
    [ "let", "in", "end"
    , "if", "then"
    , "while", "for", "to", "do", "break"
    , "of"
    ]
{-# NOINLINE reserved #-}


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
parens = between (char '(' *> sc) (sc <* char ')')
{-# INLINE parens #-}


braces :: Parser a -> Parser a
braces = between (char '{' *> sc) (sc <* char '}')
{-# INLINE braces #-}


brackets :: Parser a -> Parser a
brackets = between (char '[' *> sc) (sc <* char ']')
{-# INLINE brackets #-}


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
    A.At r _ <- lexeme $ located (chunk op)
    pure $ \e -> A.At r (f e)
{-# INLINE prefix #-}


binaryL :: Text -> (LcExpr -> LcExpr -> Expr) -> Operator Parser LcExpr
binaryL op f = InfixL $ do
    A.At r _ <- lexeme $ located (chunk op)
    pure $ \e1 e2 -> A.At r (f e1 e2)
{-# INLINE binaryL #-}


binaryN :: Text -> (LcExpr -> LcExpr -> Expr) -> Operator Parser LcExpr
binaryN op f = InfixN $ do
    A.At r _ <- lexeme $ located (chunk op)
    pure $ \e1 e2 -> A.At r (f e1 e2)
{-# INLINE binaryN #-}

-------------------------------------------------------------------------------

pLcExpr :: Parser LcExpr
pLcExpr = choice
    [ pValue
    ]


pLcTypeId :: Parser LcType
pLcTypeId = do
    i <- located ident
    pure $! NameTy <$> i


pLcVar :: Parser LcVar
pLcVar = located $ do
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
pValue = makeExprParser pPrimary table


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
{-# NOINLINE table #-}


pPrimary :: Parser LcExpr
pPrimary =
    (located $ choice
        [ UnitExpr <$ chunk "()"
        , try (NilExpr <$ keyword "nil")
        , IntExpr <$> integer
        , StrExpr <$> string
        -- FIXME: Avoid using try
        , try pRecord
        , try pArray
        , try pCall
        , VarExpr <$> pLcVar
        ])
    <|> parens pLcExpr


pCall :: Parser Expr
pCall =  do
    i <- ident
    args <- parens ((pLcExpr <* sc) `sepBy` symbol ",")
    pure (CallExpr i args)


pRecord :: Parser Expr
pRecord = do
    ty <- lexeme pLcTypeId
    fs <- braces
        $ (do
            f <- lexeme (located ident)
            _ <- symbol "="
            e <- pLcExpr <* sc
            pure (f, e))
        `sepBy`
            symbol ","
    pure (RecordExpr fs ty)


pArray :: Parser Expr
pArray = do
    ty <- lexeme pLcTypeId
    e1 <- brackets pLcExpr
    _  <- lexeme (keyword "of")
    e2 <- pLcExpr
    pure (ArrayExpr ty e1 e2)
