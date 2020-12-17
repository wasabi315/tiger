{-# LANGUAGE StrictData #-}

module Tiger.Syntax.AST
    ( Expr(..)
    , Var(..)
    , Type(..)
    , Decl(..)
    , FnDecl(..)
    , LcExpr
    , LcVar
    , LcType
    , LcDecl
    ) where

--------------------------------------------------------------------------------

import qualified Data.Text                  as T

import           Data.Symbol
import           Tiger.Reporting.Annotation

--------------------------------------------------------------------------------

type LcExpr = Located Expr
type LcVar  = Located Var
type LcType = Located Type
type LcDecl = Located Decl


data Expr
    = Nil
    | Int {-# UNPACK #-} Int
    | Str T.Text
    | Var Var
    | Call (Located Symbol) [LcExpr]
    | Unop Unop LcExpr
    | Binop Binop LcExpr LcExpr
    | Record [(Located Symbol, LcExpr)]
    | Array LcType LcExpr [LcExpr]
    | Seq [LcExpr]
    | Assign LcVar LcExpr
    | If LcExpr LcExpr (Maybe LcExpr)
    | While LcExpr LcExpr
    | For (Located Symbol) LcExpr LcExpr LcExpr
    | Break
    | Let [LcDecl] [LcExpr]
    deriving (Show)


data Var
    = VName Symbol
    | VFld LcVar (Located Symbol)
    | VIxed LcVar LcExpr
    deriving (Show)


data Unop
    = Neg
    deriving (Eq, Show)


data Binop
    = Add
    | Sub
    | Mul
    | Div
    | Eq_
    | Neq
    | Lt
    | Le
    | Gt
    | Ge
    | And
    | Or
    deriving (Eq, Show)


data Type
    = TName Symbol
    | TRecord [(Located Symbol, LcType)]
    | TArray LcType
    deriving (Show)


data Decl
    = DFn FnDecl
    | DVar (Located Symbol) (Maybe LcType) LcExpr
    | DType (Located Symbol) LcType
    deriving (Show)


data FnDecl = FnDecl
    { fdName   :: Located Symbol
    , fdParams :: [(Located Symbol, LcType)]
    , fdResult :: Maybe LcType
    , fdBody   :: LcExpr
    }
    deriving (Show)
