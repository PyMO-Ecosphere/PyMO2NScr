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
  , intercept
  , invalidArg
  ) where

import IDAllocator
import Data.Text
import qualified Data.Text.Lazy as TL
import Language.PyMO.Script
import Control.Monad.Trans.RWS
import Data.Text.Lazy.Builder
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Class (MonadTrans(..))


data CompilerEnv = CompilerEnv
  { localVars :: IDAllocator Text
  , globalVars :: IDAllocator Text
  , pymoLabels :: IDAllocator (ScriptName, Maybe Text)
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


intercept :: (Monoid w, Monad m) => RWST r w s m a -> RWST r w s m (w, a)
intercept (RWST f) = RWST $ \r s -> do
  (a', s', w') <- f r s
  return ((w', a'), s', mempty)


invalidArg :: CompilerM a
invalidArg = lift $ throwE "Invalid argument"

