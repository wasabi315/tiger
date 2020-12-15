{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData        #-}

module Tiger.Reporting.Annotation
    ( Pos
    , pos
    , unPos
    , Region (..)
    , Located (..)
    , at
    ) where

--------------------------------------------------------------------------------

import           Text.Megaparsec.Pos

--------------------------------------------------------------------------------

pos :: Int -> Pos
pos = mkPos
{-# INLINE pos #-}

--------------------------------------------------------------------------------

data Region = Region
    {-# UNPACK #-} Pos
    {-# UNPACK #-} Pos
    deriving (Eq, Show)


instance Semigroup Region where
    Region s1 e1 <> Region s2 e2 =
        Region (min s1 s2) (max e1 e2)
    {-# INLINE (<>) #-}

--------------------------------------------------------------------------------

data Located a = At {-# UNPACK #-} Region a
    deriving (Show, Functor, Foldable, Traversable)


at :: Pos -> Pos -> a -> Located a
at s e = At (Region s e)
{-# INLINE at #-}
