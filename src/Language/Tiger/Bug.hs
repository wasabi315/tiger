module Language.Tiger.Bug
  ( compilerBug,
  )
where

import GHC.Stack

compilerBug :: HasCallStack => String -> a
compilerBug msg = error $ "[COMPILER BUG]: " ++ msg
