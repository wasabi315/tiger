module Language.Tiger.Syntax.Loc
  ( Span (..),
    Located (..),
    mergeSpan,
  )
where

data Span = Span
  { start :: {-# UNPACK #-} !Int,
    end :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)

mergeSpan :: Span -> Span -> Span
mergeSpan (Span start1 end1) (Span start2 end2) =
  Span (start1 `min` start2) (end1 `max` end2)

data Located a = At Span a
  deriving (Eq, Show, Functor, Foldable, Traversable)
