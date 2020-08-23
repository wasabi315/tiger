{-# OPTIONS_GHC -Wall      #-}
{-# LANGUAGE DeriveFunctor #-}

module Tiger.Reporting.Annotation
    ( Located (..)
    , Position (..)
    , Region (..)
    , mkRegion
    , mergeRegion
    ) where


data Located a = At Region a
    deriving ( Functor )


data Position = Position
    { posnFile :: String
    , posnLine :: {-# UNPACK #-} !Int
    , posnCol  :: {-# UNPACK #-} !Int
    } deriving ( Eq, Show )


data Region = Region
    { rgnFile  :: String
    , rgnSLine :: {-# UNPACK #-} !Int
    , rgnSCol  :: {-# UNPACK #-} !Int
    , rgnELine :: {-# UNPACK #-} !Int
    , rgnECol  :: {-# UNPACK #-} !Int
    } deriving ( Eq, Show )


mkRegion :: Position -> Position -> Region
mkRegion start end = Region file sline scol eline ecol
    where
        file  = posnFile start
        sline = posnLine start
        scol  = posnCol  start
        eline = posnLine end
        ecol  = posnCol  end


mergeRegion :: Region -> Region -> Region
mergeRegion r1 r2 = Region file sline scol eline ecol
    where
        file          = rgnFile r1
        (sline, scol) = min (rgnSLine r1, rgnSCol r1) (rgnSLine r2, rgnSCol r2)
        (eline, ecol) = max (rgnELine r1, rgnECol r1) (rgnELine r2, rgnECol r2)
