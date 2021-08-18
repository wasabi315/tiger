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

import Language.Tiger.Syntax.Location qualified as Loc

type Expr = Loc.Located Expr_

data Expr_
  = Var Var
  | Int Int
  | Str String
  | Call String [Expr]
  | Op Op Expr Expr
  | Record [Loc.Located (String, Expr)] String
  | Seq [Expr]
  | Assign Var Expr
  | If Expr Expr (Maybe Expr)
  | While Expr Expr
  | For Var Expr Expr Expr
  | Break
  | Let [Decl] Expr
  | Array String Expr Expr
  deriving (Show)

type Var = Loc.Located Var_

data Var_
  = VName String
  | VField Var String
  | VIxed Var Expr
  deriving (Show)

data Decl
  = DFn [FnDecl]
  | DVar VarDecl
  | DTy [TyDecl]
  deriving (Show)

type FnDecl = Loc.Located FnDecl_

data FnDecl_ = FnDecl
  { fdName :: String,
    fdParams :: [Loc.Located (String, String)],
    fdResult :: Maybe (Loc.Located String),
    fdBody :: Expr
  }
  deriving (Show)

type VarDecl = Loc.Located VarDecl_

data VarDecl_ = VarDecl
  { vdName :: String,
    vdType :: Maybe (Loc.Located String),
    vdInit :: Expr
  }
  deriving (Show)

type TyDecl = Loc.Located TyDecl_

data TyDecl_ = TyDecl
  { tdName :: String,
    tdType :: Type
  }
  deriving (Show)

type Type = Loc.Located Type_

data Type_
  = TName String
  | TRecord
  | TArray String
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
