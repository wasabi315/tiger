module Language.Tiger.Syntax.Token
  ( Token,
    Token_ (..),
  )
where

import Language.Tiger.Syntax.Location qualified as Loc

type Token = Loc.Located Token_

data Token_
  = Type
  | Var
  | Func
  | Of
  | End
  | In
  | Nil
  | Let
  | Do
  | To
  | For
  | While
  | Else
  | Then
  | If
  | Array
  | Assign
  | Or
  | And
  | Ge
  | Gt
  | Le
  | Lt
  | Neq
  | Eq
  | Div
  | Mul
  | Sub
  | Add
  | Dot
  | RBrace
  | LBrace
  | RBrack
  | LBrack
  | RParen
  | LParen
  | Semi
  | Colon
  | Comma
  | Str String
  | Int {-# UNPACK #-} !Int
  | Id String
  | EOF
  deriving (Eq, Show)
