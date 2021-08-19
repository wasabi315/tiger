module Main where

import Data.Text.Lazy.IO qualified as T
import Language.Tiger.Syntax.Lexer qualified as Lex
import Language.Tiger.Syntax.Monad qualified as P

main :: IO ()
main = do
  s <- T.getContents
  print $ P.runParser Lex.munchAll s
