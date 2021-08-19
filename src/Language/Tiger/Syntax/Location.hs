{-# LANGUAGE PatternSynonyms #-}

module Language.Tiger.Syntax.Location
  ( Span,
    start,
    end,
    pattern Span,
    Located (..),
  )
where

-- Invariant: start s <= end s
data Span = UnsafeSpan {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Eq)

instance Show Span where
  showsPrec _ (UnsafeSpan s e) =
    showString "Span "
      . shows s
      . showChar ' '
      . shows e

start, end :: Span -> Int
start (UnsafeSpan s _) = s
end (UnsafeSpan _ e) = e

pattern Span :: Int -> Int -> Span
pattern Span s e <-
  UnsafeSpan s e
  where
    Span s e
      | s > e = error "Illegal span"
      | otherwise = UnsafeSpan s e

{-# COMPLETE Span #-}

instance Semigroup Span where
  {-# INLINE (<>) #-}
  UnsafeSpan s1 e1 <> UnsafeSpan s2 e2 = UnsafeSpan (s1 `min` s2) (e1 `max` e2)

data Located a = At
  { loc :: Span,
    value :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)
