{-# OPTIONS_GHC -Wno-unused-do-bind #-}

{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE TypeApplications #-}

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

                        -- '\001' to '\026'
                        , do
                            c <- upperChar
                            pure $! chr (ord c - ord 'A' + ord '\001')

                        , '\027' <$ char '['
                        , '\028' <$ char '\\'
                        , '\029' <$ char ']'
                        , '\030' <$ char '^'
                        , '\031' <$ char '_'
                        , '\127' <$ char '?'
                        ]

                -- ascii code: \ddd
                , do
                    o <- getOffset
                    n <- read @Int <$> replicateM 3 digitChar
                    when (n >= 128) do
                        parseError $ err o (ulabel "invalid ascii code")
                    pure $! chr n
                ]

        escSkip :: Parser ()
        escSkip = space1 <* char '\\'
