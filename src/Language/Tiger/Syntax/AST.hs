module Language.Tiger.Syntax.AST
  ( Expr,
    Expr_ (..),
    Var,
    Var_ (..),
    Decl (..),
    FnDecl,
    FnDecl_ (..),
    VarDecl,
    VarDecl_ (..),
    TyDecl,
    TyDecl_ (..),
    Type,
    Type_ (..),
    Op (..),
  )
where

import Data.Text.Lazy qualified as T
import Language.Tiger.Syntax.Location qualified as Loc

type Symbol = T.Text

type Expr = Loc.Located Expr_

data Expr_
  = Var Var
  | Nil
  | Int Int
  | Str T.Text
  | Call Symbol [Expr]
  | Op Op Expr Expr
  | Record [Loc.Located (Symbol, Expr)] Symbol
  | Seq [Expr]
  | Assign Var Expr
  | If Expr Expr (Maybe Expr)
  | While Expr Expr
  | For Var Expr Expr Expr
  | Break
  | Let [Decl] Expr
  | Array Symbol Expr Expr
  deriving (Show)

type Var = Loc.Located Var_

data Var_
  = VName Symbol
  | VField Var Symbol
  | VIxed Var Expr
  deriving (Show)

data Decl
  = DFn [FnDecl]
  | DVar VarDecl
  | DTy [TyDecl]
  deriving (Show)

type FnDecl = Loc.Located FnDecl_

data FnDecl_ = FnDecl
  { fdName :: Symbol,
    fdParams :: [Loc.Located (Symbol, Symbol)],
    fdResult :: Maybe (Loc.Located Symbol),
    fdBody :: Expr
  }
  deriving (Show)

type VarDecl = Loc.Located VarDecl_

data VarDecl_ = VarDecl
  { vdName :: Symbol,
    vdType :: Maybe (Loc.Located Symbol),
    vdInit :: Expr
  }
  deriving (Show)

type TyDecl = Loc.Located TyDecl_

data TyDecl_ = TyDecl
  { tdName :: Symbol,
    tdType :: Type
  }
  deriving (Show)

type Type = Loc.Located Type_

data Type_
  = TName Symbol
  | TRecord [Loc.Located (Symbol, Symbol)]
  | TArray Symbol
  deriving (Show)

data Op
  = Add
  | Sub
  | Mul
  | Div
  | Eq
  | Neq
  | Le
  | Lt
  | Ge
  | Gt
  deriving (Show)
