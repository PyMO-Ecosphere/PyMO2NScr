module NScrGen
  (gen, newline, defineLabel, genLine, commentLine) where

import CompilerMonad
import ToBuilder
import Control.Monad.Trans.RWS (tell, modify)


gen :: ToBuilder b => b -> Compiler
gen = tell . toBuilder


genLine :: ToBuilder b => b -> Compiler
genLine x = gen x >> newline


commentLine :: ToBuilder b => b -> Compiler
commentLine x = gen "; " >> genLine x


newline :: Compiler
newline = gen '\n'


defineLabel :: ToBuilder b => b -> Compiler
defineLabel label = do
  modify $ \x -> x { nscrLabels = nscrLabels x + 1 }
  gen "*"
  gen label
  newline

