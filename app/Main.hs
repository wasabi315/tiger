module Main (main) where

import           Tokens (tokenize)

main :: IO ()
main = do
    s <- getContents
    print $ tokenize s
