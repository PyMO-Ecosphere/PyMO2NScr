module CompilerMonad
  ( CompilerM
  , Compiler
  , CompilerEnv
    ( localVars
    , globalVars
    , cgs
    , pymoLabels
    , effects
    , currentStmt )
  , CompilerState
    ( nscrLabels )
  , runCompiler
  , intercept
  , invalidArg
  , printError
  ) where

import IDAllocator
import Data.Text
import qualified Data.Text.Lazy as TL
import Language.PyMO.Script
import Control.Monad.RWS
import Data.Text.Lazy.Builder
import Control.Monad.Except (ExceptT, runExceptT, MonadError (throwError))


data CompilerEnv = CompilerEnv
  { localVars :: IDAllocator Text
  , globalVars :: IDAllocator Text
  , cgs :: IDAllocator Text
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
    cgs' <- newIDAllocator 0
    pymoLabels' <- newIDAllocator 0
    effects' <- newIDAllocator 2
    return $ CompilerEnv
      { localVars = localVars'
      , globalVars = globalVars'
      , cgs = cgs'
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


printError :: String -> Compiler
printError errStr = do
  stmt <- asks currentStmt
  liftIO $ putStrLn $
    case stmt of
      Just stmt' ->
        let scriptName = stmtScriptName stmt'
            lineNumber = stmtLineNumber stmt' in
        scriptName ++ "(" ++ show lineNumber ++ "): " ++ errStr
      Nothing ->
        "(unknown): " ++ errStr


invalidArg :: CompilerM a
invalidArg = do
  stmt <- asks currentStmt
  throwError $
    case stmt of
      Nothing -> "Invalid arguments."
      Just stmt' -> "Call " ++ unpack (stmtCommand stmt') ++ " with invalid arguments."

