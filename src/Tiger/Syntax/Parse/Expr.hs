{-# OPTIONS_GHC -Wno-unused-do-bind #-}

{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Tiger.Syntax.Parse.Expr
    ( expr
    ) where

--------------------------------------------------------------------------------

import           Control.Monad.Combinators.Expr
import           Text.Megaparsec
import           Text.Megaparsec.Char           hiding (space)

import           Tiger.Reporting.Annotation
import           Tiger.Syntax.AST
import           Tiger.Syntax.Parse
import           Tiger.Syntax.Parse.Primitives
import           Tiger.Syntax.Parse.Unescape

--------------------------------------------------------------------------------

expr :: Parser LcExpr
expr = makeExprParser (space *> term <* space) table
    where
        table :: [[Operator Parser LcExpr]]
        table =
            [ {- highest precedence -}

              [ prefix  "-"  (Unop Neg)
              ]

            , [ infixL  "*"  (Binop Mul)
              , infixL  "/"  (Binop Div)
              ]

            , [ infixL  "+"  (Binop Add)
              , infixL  "-"  (Binop Sub)
              ]

            , [ infixN  "="  (Binop Eq_)
              , infixN  "<>" (Binop Neq)
              , infixN  ">=" (Binop Ge)
              , infixN  ">"  (Binop Gt)
              , infixN  "<=" (Binop Le)
              , infixN  "<"  (Binop Lt)
              ]

            , [ infixL  "&"  (Binop And)
              ]

            , [ infixL  "|"  (Binop Or)
              ]

              {- lowest precedence -}
            ]

        prefix op f =
            Prefix do
                At r _ <- located (string op)
                pure $ At r . f

        infixL op f =
            InfixL do
                At r _ <- located (string op)
                pure $ \el er -> At r (f el er)

        infixN op f =
            InfixL do
                At r _ <- located (string op)
                pure $ \el er -> At r (f el er)

        _infixR op f =
            InfixL do
                At r _ <- located (string op)
                pure $ \el er -> At r (f el er)


--------------------------------------------------------------------------------

term :: Parser LcExpr
term =
    choice
        [ let_
        , if_
        , while
        , for
        , break_
        , seq_
        , try assign
        , try literal
        , located (Var <$> var)
        ]

--------------------------------------------------------------------------------

literal :: Parser LcExpr
literal =
    choice
        [ located (Nil <$ keyword "nil")
        , located (Int <$> int)
        , located (Str <$> str)
        , arrayOrRecord
        ]


arrayOrRecord :: Parser LcExpr
arrayOrRecord =
    located do
        ty <- located ident
        __
        choice
            [ do
                e1 <- brackets (space *> expr <* space)
                __
                keyword "of"
                __
                e2 <- expr
                pure $ Array ty e1 e2

            , do
                fs <- braces $ (space *> field <* space) `sepBy` comma
                pure $ Record ty fs
            ]

    where
        field = do
            fname <- located ident
            __
            keyword "="
            __
            e <- expr
            pure (fname, e)

--------------------------------------------------------------------------------

var :: Parser Var
var = do
    lcv <- located (VName <$> ident)
    rest lcv

    where
        rest :: LcVar -> Parser Var
        rest lcv@(At r1 v) =
            choice
                [ do
                    dot
                    f@(At r2 _) <- located ident
                    rest $ At (r1 <> r2) (VField lcv f)

                , do
                    At r2 e <- located $ brackets (space *> expr <* space)
                    rest $ At (r1 <> r2) (VIxed lcv e)

                , pure v
                ]

--------------------------------------------------------------------------------

let_ :: Parser LcExpr
let_ =
    located do
        keyword "let"
        __
        ds <- many (decl <* space)
        keyword "in"
        __
        es <- expr `sepBy` semi
        __
        keyword "end"
        pure $ Let ds es


decl :: Parser LcDecl
decl =
    choice
        []

--------------------------------------------------------------------------------

if_ :: Parser LcExpr
if_ =
    located do
        keyword "if"
        __
        e1 <- expr
        __
        keyword "then"
        __
        e2 <- expr
        e3 <- optional do
            __
            keyword "else"
            __
            expr
        pure $ If e1 e2 e3

--------------------------------------------------------------------------------

while :: Parser LcExpr
while =
    located do
        keyword "while"
        __
        e1 <- expr
        __
        keyword "do"
        __
        e2 <- expr
        pure $ While e1 e2


for :: Parser LcExpr
for =
    located do
        keyword "for"
        __
        s <- located ident
        __
        keyword ":="
        __
        e1 <- expr
        __
        keyword "to"
        __
        e2 <- expr
        __
        keyword "do"
        __
        e3 <- expr
        pure $ For s e1 e2 e3


break_ :: Parser LcExpr
break_ = located $ Break <$ keyword "break"

--------------------------------------------------------------------------------

seq_ :: Parser LcExpr
seq_ = located $ parens (Seq <$> expr `sepBy` semi)

--------------------------------------------------------------------------------

assign :: Parser LcExpr
assign =
    located do
        v <- located var
        __
        keyword ":="
        __
        e <- expr
        pure $ Assign v e
