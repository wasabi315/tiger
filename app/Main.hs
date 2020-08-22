module Main where

import Tokens (tokenize)

main :: IO ()
main = do
  s <- getContents
  print $ tokenize s
