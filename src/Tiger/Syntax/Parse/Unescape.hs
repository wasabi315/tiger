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
                    choice
                        [ '\000' <$ char '@'
                        , '\001' <$ char 'A'
                        , '\002' <$ char 'B'
                        , '\003' <$ char 'C'
                        , '\004' <$ char 'D'
                        , '\005' <$ char 'E'
                        , '\006' <$ char 'F'
                        , '\007' <$ char 'G'
                        , '\008' <$ char 'H'
                        , '\009' <$ char 'I'
                        , '\010' <$ char 'J'
                        , '\011' <$ char 'K'
                        , '\012' <$ char 'L'
                        , '\013' <$ char 'M'
                        , '\014' <$ char 'N'
                        , '\015' <$ char 'O'
                        , '\016' <$ char 'P'
                        , '\017' <$ char 'Q'
                        , '\018' <$ char 'R'
                        , '\019' <$ char 'S'
                        , '\020' <$ char 'T'
                        , '\021' <$ char 'U'
                        , '\022' <$ char 'V'
                        , '\023' <$ char 'W'
                        , '\024' <$ char 'X'
                        , '\025' <$ char 'Y'
                        , '\026' <$ char 'Z'
                        , '\027' <$ char '['
                        , '\028' <$ char '\\'
                        , '\029' <$ char ']'
                        , '\030' <$ char '^'
                        , '\031' <$ char '_'
                        , '\127' <$ char '?'
                        ]

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
