{-# LANGUAGE PatternSynonyms #-}

module Language.Tiger.Syntax.Location
  ( Span,
    start,
    end,
    startSpan,
    endSpan,
    pattern Span,
    Located (..),
    toSpan,
    merge,
  )
where

import Control.Comonad
import Language.Tiger.Bug qualified as Bug

-- Invariant: start s <= end s
data Span = UnsafeSpan {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Eq)

instance Show Span where
  showsPrec prec (UnsafeSpan s e) =
    showParen (prec > 10) $
      showString "Span "
        . shows s
        . showChar ' '
        . shows e

start, end :: Span -> Int
start (UnsafeSpan s _) = s
end (UnsafeSpan _ e) = e

startSpan, endSpan :: Span -> Span
startSpan (UnsafeSpan s _) = UnsafeSpan s s
endSpan (UnsafeSpan _ e) = UnsafeSpan e e

pattern Span :: Int -> Int -> Span
pattern Span s e <-
  UnsafeSpan s e
  where
    Span s e
      | s > e = Bug.compilerBug "Illegal span"
      | otherwise = UnsafeSpan s e

{-# COMPLETE Span #-}

instance Semigroup Span where
  {-# INLINE (<>) #-}
  UnsafeSpan s1 e1 <> UnsafeSpan s2 e2 = UnsafeSpan (s1 `min` s2) (e1 `max` e2)

data Located a = At Span a
  deriving (Eq, Show, Functor, Foldable, Traversable)

toSpan :: Located a -> Span
toSpan (At sp _) = sp

instance Comonad Located where
  {-# INLINE extract #-}
  extract (At _ a) = a

  {-# INLINE extend #-}
  extend f w@(At sp _) = At sp (f w)

instance ComonadApply Located where
  {-# INLINE (<@>) #-}
  At sp1 f <@> At sp2 a = At (sp1 <> sp2) (f a)

  {-# INLINE (<@) #-}
  At sp1 a <@ At sp2 _ = At (sp1 <> sp2) a

  {-# INLINE (@>) #-}
  At sp1 _ @> At sp2 a = At (sp1 <> sp2) a

merge :: Located a -> Located b -> c -> Located c
merge (At sp1 _) (At sp2 _) = At (sp1 <> sp2)
