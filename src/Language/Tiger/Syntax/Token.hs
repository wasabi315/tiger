module Language.Tiger.Syntax.Token
  ( Token,
    Token_ (..),
  )
where

import Data.Text.Lazy qualified as T
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
  | Break
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
  | Str T.Text
  | Int {-# UNPACK #-} !Int
  | Id T.Text
  | EOF
  deriving (Eq, Show)
