{-# OPTIONS_GHC -Wall #-}

module Tiger.Parsing.Unescape
    ( strLit
    ) where

import           Data.Text                (Text)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Tiger.Parsing.Primitives (Parser)

--------------------------------------------------------------------------------

strLit :: Parser Text
strLit = between (char '\"') (char '\"') (takeWhile1P Nothing (/= '"'))
