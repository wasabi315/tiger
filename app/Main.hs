module Main where

import Data.Text.Lazy.IO qualified as T
import Language.Tiger.Syntax.Monad qualified as P
import Language.Tiger.Syntax.Parser qualified as P

main :: IO ()
main = do
  s <- T.getContents
  print $ P.runParser P.tiger s
