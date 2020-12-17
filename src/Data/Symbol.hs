{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Data.Symbol
    ( Symbol
    , symbol
    , name
    ) where

--------------------------------------------------------------------------------

import           Data.Hashable
import           Data.String
import qualified Data.Text     as T

--------------------------------------------------------------------------------

newtype Symbol = Symbol T.Text
    deriving stock (Eq, Show)
    deriving newtype (IsString, Hashable)

--------------------------------------------------------------------------------

symbol :: T.Text -> Symbol
symbol = Symbol
{-# INLINE symbol #-}


name :: Symbol -> T.Text
name (Symbol s) = s
{-# INLINE name #-}
