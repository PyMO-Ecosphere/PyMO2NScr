module NScrGen
  (gen, newline, defineLabel) where

import CompilerMonad
import ToBuilder
import Control.Monad.Trans.RWS (tell, modify)


gen :: ToBuilder b => b -> Compiler
gen = tell . toBuilder


newline :: Compiler
newline = gen '\n'


defineLabel :: ToBuilder b => b -> Compiler
defineLabel label = do
  modify $ \x -> x { nscrLabels = nscrLabels x + 1 }
  gen "*"
  gen label

