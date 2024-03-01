module CompilerMonad
  ( CompilerM
  , Compiler
  , CompilerEnv
    ( localVars
    , globalVars
    , pymoLabels
    , effects
    , currentStmt )
  , CompilerState
    ( nscrLabels )
  , runCompiler
  ) where

import IDAllocator
import Data.Text
import qualified Data.Text.Lazy as TL
import Language.PyMO.Script
import Control.Monad.Trans.RWS
import Data.Text.Lazy.Builder
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


runCompiler :: Compiler -> IO TL.Text
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
  return $ toLazyText $ either error (\((), _, b) -> b) result

