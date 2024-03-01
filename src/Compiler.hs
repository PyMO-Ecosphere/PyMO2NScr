module Compiler
  ( compile ) where

import CompilerMonad
import NScrGen


compile :: FilePath -> Compiler
compile gameDir = do
  defineLabel "define"
  gen "game"
  newline
  defineLabel "start"

