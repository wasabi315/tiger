module Main where

import Language.Tiger.Syntax.Lexer qualified as Lex

main :: IO ()
main = do
  s <- getContents
  print $ Lex.lex s
