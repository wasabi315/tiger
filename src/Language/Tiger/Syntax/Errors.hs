module Language.Tiger.Syntax.Errors
  ( Error,
    ErrorKind (..),
  )
where

import Data.Text.Lazy qualified as T
import Language.Tiger.Syntax.Location qualified as Loc
import Language.Tiger.Syntax.Token qualified as Tok

type Error = Loc.Located ErrorKind

data ErrorKind
  = UnclosedString
  | IllegalEscapeSequence T.Text
  | IllegalAsciiValue Int
  | NewlineInString
  | UnclosedComment
  | IllegalCommentClose
  | UnknownToken T.Text
  | UnknownLexicalError
  | ParseError Tok.Token_ [String]
  deriving (Show)
