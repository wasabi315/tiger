{-# OPTIONS_GHC -Wall      #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData    #-}

module Tiger.Reporting.Annotation
    ( Located (..)
    , Region (..)
    , located
    ) where


import           Text.Megaparsec (SourcePos)


data Located a = At Region a
    deriving ( Functor, Eq, Show )


data Region = Region SourcePos SourcePos
     deriving ( Eq, Show )


located :: SourcePos -> SourcePos -> a -> Located a
located s e a = At (Region s e) a
{-# INLINE located #-}
