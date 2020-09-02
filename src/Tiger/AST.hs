{-# OPTIONS_GHC -Wall   #-}
{-# LANGUAGE StrictData #-}

module Tiger.AST
    ( Symbol
    , Expr, Expr_ (..)
    , Dec, Dec_ (..)
    , Var, Var_ (..)
    , Func, Func_ (..)
    , Type, Type_ (..)
    , BinOp (..), UnOp (..)
    ) where

import           Data.Text                  (Text)

import qualified Tiger.Reporting.Annotation as A


type Symbol = Text


type Expr = A.Located Expr_

data Expr_
    = VarExpr Var
    | NilExpr
    | NoValueExpr
    | IntExpr {-# UNPACK #-} Int
    | StrExpr Text
    | CallExpr Symbol [Expr]
    | UnOpExpr UnOp Expr
    | BinOpExpr BinOp Expr Expr
    | RecordExpr [(A.Located Symbol, Expr)] Type
    | SeqExp [Expr]
    | AssignExpr Var Expr
    | IfExpr Expr Expr (Maybe Expr)
    | WhileExpr Expr Expr
    | ForExpr (A.Located Symbol) Expr Expr Expr
    | BreakExpr
    | LetExpr [Dec] Expr
    | ArrayExpr Type Expr Expr
    deriving ( Eq, Show )


type Dec = A.Located Dec_

data Dec_
    = FuncDec [Func]
    | VarDec Symbol (Maybe Type) Expr
    | TypeDec [(A.Located Symbol, Type)]
    deriving ( Eq, Show )


type Var = A.Located Var_

data Var_
    = SimpleVar Symbol
    | FieldVar Var_ (A.Located Symbol)
    | SubscriptVar Var_ Expr
    deriving ( Eq, Show )


type Func = A.Located Func_

data Func_ = Func
    { name   :: Symbol
    , params :: [(A.Located Symbol, Type)]
    , result :: Maybe Type
    , body   :: Expr
    } deriving ( Eq, Show )


type Type = A.Located Type_

data Type_
    = NameTy Symbol
    | RecordTy [(A.Located Symbol, Type)]
    | ArrayTy Type
    deriving ( Eq, Show )

data BinOp
    = PlusOp
    | MinusOp
    | MulOp
    | DivOp
    | EqOp
    | NeqOp
    | LtOp
    | LeOp
    | GtOp
    | GeOp
    | AndOp
    | OrOp
    deriving ( Eq, Show )

data UnOp
    = NegOp
    deriving ( Eq, Show )
