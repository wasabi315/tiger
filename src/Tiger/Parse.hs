{-# OPTIONS_GHC -Wall -Wno-unused-do-bind #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Tiger.Parse
    ( Tiger.Parse.parse
    ) where

import           Control.Exception              (SomeException (..))
import           Control.Monad.Combinators.Expr
import           Data.Char
import           Data.Set                       (Set)
import qualified Data.Set                       as S
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L
import           Text.Megaparsec.Error.Builder

import           Tiger.AST
import qualified Tiger.Reporting.Annotation     as A

-------------------------------------------------------------------------------

parse :: Text -> Either SomeException LcExpr
parse input = case Text.Megaparsec.parse pTiger "" input of
    Left  perr -> Left (SomeException perr)
    Right expr -> Right expr

-------------------------------------------------------------------------------

type Parser = Parsec Void Text

-------------------------------------------------------------------------------

__ :: Parser ()
__ = L.space space1 empty (L.skipBlockComment "/*" "*/")
{-# INLINE __ #-}


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
    , "if", "then", "else"
    , "while", "for", "to", "do", "break"
    , "array", "of"
    ]
{-# NOINLINE reserved #-}


intLit :: Parser Int
intLit = L.decimal
{-# INLINE intLit #-}


strLit :: Parser Text
strLit = between (char '\"') (char '\"') (takeWhile1P Nothing (/= '"'))
{-# INLINE strLit #-}


keyword :: Text -> Parser Text
keyword k = try $ string k <* notFollowedBy (satisfy isValidIdentChar)
{-# INLINE keyword #-}


parens :: Parser a -> Parser a
parens = between (char '(' *> __) (__ <* char ')')
{-# INLINE parens #-}


braces :: Parser a -> Parser a
braces = between (char '{' *> __) (__ <* char '}')
{-# INLINE braces #-}


brackets :: Parser a -> Parser a
brackets = between (char '[' *> __) (__ <* char ']')
{-# INLINE brackets #-}


pad :: Parser a -> Parser a
pad = between __ __
{-# INLINE pad #-}

-------------------------------------------------------------------------------

located :: Parser a -> Parser (A.Located a)
located p = do
    s <- getOffset
    a <- p
    e <- getOffset
    pure $! A.located s e a
{-# INLINE located #-}


prefix :: Text -> (LcExpr -> Expr) -> Operator Parser LcExpr
prefix op f = Prefix do
    A.At r _ <- try . pad $ located (string op)
    pure $ \e -> A.At r (f e)
{-# INLINE prefix #-}


binaryL :: Text -> (LcExpr -> LcExpr -> Expr) -> Operator Parser LcExpr
binaryL op f = InfixL do
    A.At r _ <- try . pad $ located (string op)
    pure $ \e1 e2 -> A.At r (f e1 e2)
{-# INLINE binaryL #-}


binaryN :: Text -> (LcExpr -> LcExpr -> Expr) -> Operator Parser LcExpr
binaryN op f = InfixN do
    A.At r _ <- try . pad $ located (string op)
    pure $ \e1 e2 -> A.At r (f e1 e2)
{-# INLINE binaryN #-}


sepBySpaces1 :: Parser a -> Parser [a]
sepBySpaces1 p = (:) <$> p <*> many (try $ __ *> p)
{-# INLINE sepBySpaces1 #-}

-------------------------------------------------------------------------------

pTiger :: Parser LcExpr
pTiger = pad pLcExpr <* eof
{-# INLINE pTiger #-}


pLcExpr :: Parser LcExpr
pLcExpr = choice
    [ pLet
    , pIf
    , pWhile
    , pFor
    , located (BreakExpr <$ keyword "break")
    , try pSeq
    , try pAssign
    , pValue
    ]


pLet :: Parser LcExpr
pLet = located do
    keyword "let"
    __
    decs  <- many (pLcDec <* __)
    keyword "in"
    __
    exprs <- (pLcExpr <* __) `sepBy` (char ';' <* __)
    keyword "end"
    pure (LetExpr decs exprs)


pLcDec :: Parser LcDec
pLcDec = choice
    [ pLcTypeDec
    , pLcVarDec
    , pLcFuncDec
    ]


pLcTypeDec :: Parser LcDec
pLcTypeDec = TypeDec <$> sepBySpaces1 do
    keyword "type"
    __
    tname <- located ident
    __
    char '='
    __
    ty    <- pLcType
    pure (tname, ty)


pLcType :: Parser LcType
pLcType = choice
    [ pLcTypeId
    , located (RecordTy <$> braces pTyFields)
    , located do
        keyword "array"
        __
        keyword "of"
        __
        tname <- pLcTypeId
        pure (ArrayTy tname)
    ]


pTyFields :: Parser [(A.Located Symbol, LcType)]
pTyFields =
    do
        fname <- located ident
        __
        char ':'
        __
        tname <- pLcTypeId
        __
        pure (fname, tname)
    `sepBy1`
        (char ',' <* __)


pLcVarDec :: Parser LcDec
pLcVarDec = do
    keyword "var"
    __
    vname  <- located ident
    __
    result <- optional $ try do
        char ':'
        __
        pLcTypeId
    __
    string ":="
    __
    expr   <- pLcExpr
    pure (VarDec vname result expr)


pLcFuncDec :: Parser LcDec
pLcFuncDec = FuncDec <$> (sepBySpaces1 . located) do
    keyword "function"
    __
    name   <- located ident
    __
    params <- parens pTyFields
    __
    result <- optional do
        char ':'
        __
        pLcTypeId
    __
    char '='
    __
    body <- pLcExpr
    pure Func {..}


pIf :: Parser LcExpr
pIf = located do
    keyword "if"
    __
    expr1 <- pLcExpr
    __
    keyword "then"
    __
    expr2 <- pLcExpr
    __
    expr3 <- optional do
        keyword "else"
        __
        pLcExpr
    pure (IfExpr expr1 expr2 expr3)


pWhile :: Parser LcExpr
pWhile = located do
    keyword "while"
    __
    expr1 <- pLcExpr
    __
    keyword "do"
    __
    expr2 <- pLcExpr
    pure (WhileExpr expr1 expr2)


pFor :: Parser LcExpr
pFor = located do
    keyword "for"
    __
    vname <- located ident
    __
    string ":="
    __
    expr1 <- pLcExpr
    __
    keyword "to"
    __
    expr2 <- pLcExpr
    __
    keyword "do"
    __
    expr3 <- pLcExpr
    pure (ForExpr vname expr1 expr2 expr3)


pSeq :: Parser LcExpr
pSeq = located $ SeqExpr <$> parens ((pLcExpr <* __) `sepBy` (char ';' <* __))


pAssign :: Parser LcExpr
pAssign = located do
    lvalue <- pLcVar
    __
    string ":="
    __
    expr   <- pLcExpr
    pure (AssignExpr lvalue expr)


pLcTypeId :: Parser LcType
pLcTypeId = do
    tname <- located ident
    pure $! NameTy <$> tname


pLcVar :: Parser LcVar
pLcVar = located do
    vname <- ident
    rest (NameVar vname)
    where
        rest v = choice
            [ do
                char '.'
                fname <- located ident
                rest (FieldVar v fname)
            , brackets do
                iexpr <- pLcExpr
                rest (IdxedVar v iexpr)
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
    (located . choice)
        [ NilExpr <$  keyword "nil"
        , IntExpr <$> intLit
        , StrExpr <$> strLit
        , try pRecord
        , try pArray
        , try pCall
        , VarExpr <$> pLcVar
        ]
    <|> parens pLcExpr


pCall :: Parser Expr
pCall = do
    fname <- ident
    args  <- parens ((pLcExpr <* __) `sepBy` (char ',' <* __))
    pure (CallExpr fname args)


pRecord :: Parser Expr
pRecord = do
    ty     <- pLcTypeId
    __
    fields <- braces
        (do
            fname <- located ident
            __
            char '='
            __
            expr  <- pLcExpr
            __
            pure (fname, expr)
        `sepBy`
            (char ',' *> __))
    pure (RecordExpr fields ty)


pArray :: Parser Expr
pArray = do
    ty    <- pLcTypeId
    __
    expr1 <- brackets pLcExpr
    __
    keyword "of"
    __
    expr2 <- pLcExpr
    pure (ArrayExpr ty expr1 expr2)
