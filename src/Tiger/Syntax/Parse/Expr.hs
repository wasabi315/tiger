{-# OPTIONS_GHC -Wno-unused-do-bind #-}

{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Tiger.Syntax.Parse.Expr
    ( expr
    ) where

--------------------------------------------------------------------------------

import           Control.Monad.Combinators.Expr
import           Text.Megaparsec.Char           hiding (space)

import           Tiger.Reporting.Annotation
import           Tiger.Syntax.AST
import           Tiger.Syntax.Parse
import           Tiger.Syntax.Parse.Primitives

--------------------------------------------------------------------------------

expr :: Parser LcExpr
expr = ops

--------------------------------------------------------------------------------

ops :: Parser LcExpr
ops = makeExprParser (space *> located (Int <$> int) <* space) table
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

            , [ infixN  "="  (Binop Eq')
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
