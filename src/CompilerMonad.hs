module CompilerMonad
  ( CompilerM
  , Compiler
  , runCompiler
  ) where

import IDAllocator
import Data.Text
import Language.PyMO.Script
import Control.Monad.Trans.RWS
import Text.Builder
import Control.Monad.Trans.Except (ExceptT, runExceptT)


data CompilerEnv = CompilerEnv
  { localVars :: IDAllocator Text
  , globalVars :: IDAllocator Text
  , pymoLabels :: IDAllocator Text
  , effects :: IDAllocator (Text, Int)
  , currentStmt :: Maybe Stmt }


newtype CompilerState = CompilerState
  { nscrLabels :: Int }


type CompilerM = RWST CompilerEnv Builder CompilerState (ExceptT String IO)
type Compiler = CompilerM ()


runCompiler :: Compiler -> IO Builder
runCompiler x = do
  defaultEnv <- do
    localVars' <- newIDAllocator 0
    globalVars' <- newIDAllocator 0
    pymoLabels' <- newIDAllocator 0
    effects' <- newIDAllocator 0
    return $ CompilerEnv
      { localVars = localVars'
      , globalVars = globalVars'
      , pymoLabels = pymoLabels'
      , effects = effects'
      , currentStmt = Nothing }

  let defaultState = CompilerState
        { nscrLabels = 0 }

  result <- runExceptT $ runRWST x defaultEnv defaultState
  return $ either error (\((), _, b) -> b) result

