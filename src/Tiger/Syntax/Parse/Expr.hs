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
        [ literal
        , if_
        , while
        , break_
        , seq_
        ]

--------------------------------------------------------------------------------

literal :: Parser LcExpr
literal =
    choice
        [ located (Nil <$ keyword "nil")
        , located (Int <$> int)
        , located (Str <$> str)
        , array
        ]


array :: Parser LcExpr
array = located do
    ty <- located ident
    __
    e1 <- brackets (space *> expr <* space)
    __
    keyword "of"
    __
    e2 <- expr

    pure $ Array ty e1 e2


--------------------------------------------------------------------------------

if_ :: Parser LcExpr
if_ = located do
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
while = located do
    keyword "while"
    __
    e1 <- expr
    __
    keyword "do"
    __
    e2 <- expr

    pure $ While e1 e2


break_ :: Parser LcExpr
break_ = located $ Break <$ keyword "break"

--------------------------------------------------------------------------------

seq_ :: Parser LcExpr
seq_ = located $ parens (Seq <$> expr `sepBy` semi)
