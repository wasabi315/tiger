{-# OPTIONS_GHC -Wall      #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData    #-}

module Tiger.Reporting.Annotation
    ( Located (..)
    , Region (..)
    , located
    ) where


data Located a = At Region a
    deriving ( Functor, Eq, Show )


type Pos = Int


data Region = Region
    {-# UNPACK #-} Pos
    {-# UNPACK #-} Pos
    deriving ( Eq, Show )


located :: Pos -> Pos -> a -> Located a
located s e a = At (Region s e) a
{-# INLINE located #-}
