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
pLcExpr =
    (located $ choice
        [ try pLet
        , try pIf
        , try pWhile
        , try pFor
        , try (BreakExpr <$ keyword "break")
        , try pSeq
        , try pAssign
        ])
    <|> pValue


pLet :: Parser Expr
pLet = do
    _  <- lexeme (keyword "let")
    ds <- many (pLcDec <* sc)
    _  <- lexeme (keyword "in")
    es <- (pLcExpr <* sc) `sepBy` symbol ";"
    _  <- lexeme (keyword "end")
    pure (LetExpr ds es)


pLcDec :: Parser LcDec
pLcDec = choice
    [ pLcTypeDec
    , pLcVarDec
    , pLcFuncDec
    ]


pLcTypeDec :: Parser LcDec
pLcTypeDec = located $ TypeDec <$> some
    ((do
        _  <- lexeme (keyword "type")
        li <- lexeme (located ident)
        _  <- symbol "="
        ty <- pLcType
        pure (li, ty))
    <* sc)


pLcType :: Parser LcType
pLcType = choice
    [ pLcTypeId
    , located $ RecordTy <$> braces pTyFields
    , located $ do
        _ <- lexeme (keyword "array")
        _ <- lexeme (keyword "of")
        t <- pLcTypeId
        pure (ArrayTy t)
    ]


pLcVarDec :: Parser LcDec
pLcVarDec = located $ do
    _ <- lexeme (keyword "var")
    i <- lexeme ident
    t <- optional . try $ do
        _ <- symbol ":"
        lexeme pLcTypeId
    _ <- symbol ":="
    e <- pLcExpr
    pure (VarDec i t e)


pLcFuncDec :: Parser LcDec
pLcFuncDec = located $ FuncDec <$> some
    (located (do
        _  <- lexeme (keyword "function")
        i  <- lexeme ident
        ps <- parens pTyFields <* sc
        t  <- optional $ do
            _ <- symbol ":"
            lexeme pLcTypeId
        _  <- symbol "="
        e  <- pLcExpr
        pure (Func i ps t e))
    <* sc)


pTyFields :: Parser [(A.Located Symbol, LcType)]
pTyFields =
    do
        li <- lexeme (located ident)
        _  <- symbol ":"
        ty <- pLcTypeId <* sc
        pure (li, ty)
    `sepBy1`
        symbol ","


pIf :: Parser Expr
pIf = do
    _  <- lexeme (keyword "if")
    e1 <- pLcExpr <* sc
    _  <- lexeme (keyword "then")
    e2 <- pLcExpr <* sc
    e3 <- optional $ do
        _ <- lexeme (keyword "else")
        pLcExpr
    pure (IfExpr e1 e2 e3)


pWhile :: Parser Expr
pWhile = do
    _  <- lexeme (keyword "while")
    e1 <- pLcExpr <* sc
    _  <- lexeme (keyword "do")
    e2 <- pLcExpr
    pure (WhileExpr e1 e2)


pFor :: Parser Expr
pFor = do
    _  <- lexeme (keyword "for")
    i  <- lexeme (located ident)
    _  <- symbol ":="
    e1 <- pLcExpr <* sc
    _  <- lexeme (keyword "to")
    e2 <- pLcExpr <* sc
    _  <- lexeme (keyword "do")
    e3 <- pLcExpr
    pure (ForExpr i e1 e2 e3)


pSeq :: Parser Expr
pSeq = SeqExp <$> parens ((pLcExpr <* sc) `sepBy1` symbol ";")


pAssign :: Parser Expr
pAssign = do
    lv <- pLcVar <* sc
    _  <- symbol ":="
    rv <- pLcExpr
    pure (AssignExpr lv rv)


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
pValue = makeExprParser (pPrimary <* sc) table


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
