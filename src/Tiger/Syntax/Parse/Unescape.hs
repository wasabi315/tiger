{-# OPTIONS_GHC -Wno-unused-do-bind #-}

{-# LANGUAGE BlockArguments #-}

module Tiger.Syntax.Parse.Unescape
    ( str
    ) where

--------------------------------------------------------------------------------

import           Control.Monad
import           Data.Char
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Builder        as TLB
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Error.Builder

import           Tiger.Syntax.Parse
import           Tiger.Syntax.Parse.Primitives

--------------------------------------------------------------------------------

str :: Parser T.Text
str = dquotes (go mempty)
    where
        go :: TLB.Builder -> Parser T.Text
        go b =
            choice
                [ do
                    t <- takeWhile1P Nothing (`notElem` ("\"\\" :: String))
                    go (b <> TLB.fromText t)

                , do
                    char '\\'
                    choice
                        [ do
                            c <- escChar
                            go (b <> TLB.singleton c)
                        , escSkip *> go b
                        ]

                , pure $! TL.toStrict (TLB.toLazyText b)
                ]

        escChar :: Parser Char
        escChar =
            choice
                [ '\n' <$ char 'n'
                , '\t' <$ char 't'
                , '\"' <$ char '\"'
                , '\\' <$ char '\\'

                -- control character: \^c
                , do
                    char '^'
                    c <- upperChar
                    pure $! chr (ord c - ord 'A' + 1)

                -- ascii code: \ddd
                , do
                    o  <- getOffset

                    d1 <- digitChar
                    d2 <- digitChar
                    d3 <- digitChar
                    let n = read (d1:d2:d3:"") :: Int

                    when (n >= 128) do
                        parseError $ err o (ulabel "invalid ascii code")
                    pure $! chr n
                ]

        escSkip :: Parser ()
        escSkip =
            void do
                takeWhile1P Nothing (`elem` (" \t\n\f" :: String))
                char '\\'
