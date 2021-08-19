module Language.Tiger.Syntax.Errors
  ( Error,
    ErrorKind (..),
  )
where

import Data.Text.Lazy qualified as T
import Language.Tiger.Syntax.Location qualified as Loc

type Error = Loc.Located ErrorKind

data ErrorKind
  = UnclosedString
  | IllegalEscapeSequence T.Text
  | IllegalAsciiValue Int
  | NewlineInString
  | UnclosedComment
  | IllegalCommentClose
  | UnknownToken T.Text
  | UnknownLexicalIssue
  deriving (Show)
