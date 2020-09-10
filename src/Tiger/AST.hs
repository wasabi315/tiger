{-# OPTIONS_GHC -Wall   #-}
{-# LANGUAGE StrictData #-}

module Tiger.AST
    ( Symbol
    , LcExpr, Expr (..)
    , LcDec (..)
    , LcVar, Var (..)
    , LcFunc, Func (..)
    , LcType, Type (..)
    , Bop (..)
    , Uop (..)
    ) where

import           Data.Text                  (Text)

import qualified Tiger.Reporting.Annotation as A

-------------------------------------------------------------------------------

type Symbol = Text


type LcExpr = A.Located Expr
type LcFunc = A.Located Func
type LcVar  = A.Located Var
type LcType = A.Located Type


data Expr
    = NilExpr
    | IntExpr {-# UNPACK #-} Int
    | StrExpr Text
    | VarExpr LcVar
    | CallExpr Symbol [LcExpr]
    | UopExpr Uop LcExpr
    | BopExpr Bop LcExpr LcExpr
    | RecordExpr [(A.Located Symbol, LcExpr)] LcType
    | ArrayExpr LcType LcExpr LcExpr
    | SeqExpr [LcExpr]
    | AssignExpr LcVar LcExpr
    | IfExpr LcExpr LcExpr (Maybe LcExpr)
    | WhileExpr LcExpr LcExpr
    | ForExpr (A.Located Symbol) LcExpr LcExpr LcExpr
    | BreakExpr
    | LetExpr [LcDec] [LcExpr]
    deriving ( Eq, Show )


data Bop
    = AddOp
    | SubOp
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


data Uop
    = NegOp
    deriving ( Eq, Show )


data LcDec
    = FuncDec [LcFunc]
    | VarDec (A.Located Symbol) (Maybe LcType) LcExpr
    | TypeDec [(A.Located Symbol, LcType)]
    deriving ( Eq, Show )


data Var
    = NameVar Symbol
    | FieldVar Var (A.Located Symbol)
    | IdxedVar Var LcExpr
    deriving ( Eq, Show )


data Func = Func
    { name   :: A.Located Symbol
    , params :: [(A.Located Symbol, LcType)]
    , result :: Maybe LcType
    , body   :: LcExpr
    } deriving ( Eq, Show )


data Type
    = NameTy Symbol
    | RecordTy [(A.Located Symbol, LcType)]
    | ArrayTy LcType
    deriving ( Eq, Show )
