{-# LANGUAGE OverloadedStrings #-}

module Compiler
  ( compile ) where

import CompilerMonad
import NScrGen
import Language.PyMO.Game
import qualified Language.PyMO.GameConfig as GC
import Control.Monad.IO.Class (liftIO)
import Language.PyMO.Script
import Control.Monad (forM_)
import Commands
import qualified Data.HashMap.Strict as HM
import Control.Monad.Except (MonadError(catchError))
import Data.Text (unpack)
import Control.Monad.RWS ( local, gets, asks )
import IDAllocator (allocatedIDCount)


compileScript :: ScriptName -> Script -> Compiler
compileScript scriptName script = do
  scriptLabel <- pymoLabelToNScrLabel scriptName Nothing
  newline
  commentLine $ "PyMO Script: " ++ scriptName
  defineLabel scriptLabel

  forM_ script $ \stmt ->
    local (\x -> x { currentStmt = Just stmt }) $
      case HM.lookup (stmtCommand stmt) commands of
        Nothing ->
          printError $ "Could not handle command " ++ unpack (stmtCommand stmt)
        Just command ->
          catchError (command $ stmtArgs stmt) printError
  genLine "end"


runtimeVarCount :: Int
runtimeVarCount = 30


data VariableAllocation = VariableAllocation
  { firstLocalVariable :: Int
  , firstGlobalVariable :: Int
  , firstCGUnlockingVariable :: Int
  , allVariables :: Int }


allocateVariables :: CompilerEnv -> IO VariableAllocation
allocateVariables env = do
  localVariableCount <- allocatedIDCount $ localVars env
  globalVariableCount <- allocatedIDCount $ globalVars env
  cgUnlockingVariableCount <- allocatedIDCount $ cgs env
  return $
    VariableAllocation
      { firstLocalVariable =
          runtimeVarCount
      , firstGlobalVariable =
          runtimeVarCount + localVariableCount
      , firstCGUnlockingVariable =
          runtimeVarCount
          + localVariableCount
          + globalVariableCount
      , allVariables =
          runtimeVarCount
          + localVariableCount
          + globalVariableCount
          + cgUnlockingVariableCount }


compile :: FilePath -> Compiler
compile gameDir' = do
  game <- liftIO $ loadGame gameDir'
  let gameConfig' = gameConfig game

  (startSection, ()) <-
    intercept $ forM_ (gameScripts game) $ uncurry compileScript

  varAlloc <- asks allocateVariables >>= liftIO

  (defineSection, ()) <- intercept $ do
    defineLabel "define"
    cmd "caption" [Str $ GC.getTextValue "gametitle" gameConfig']

    newline

    cmd "game" []

    defineLabel "start"

    let startScript = GC.getStringValue "startscript" gameConfig'
    startNScrLabel <- pymoLabelToNScrLabel startScript Nothing
    cmd "goto" [Label startNScrLabel]
    genLine startNScrLabel

  gen ";$V"
  gen' $ allVariables varAlloc
  gen "G"
  gen' $ firstGlobalVariable varAlloc
  gen "S"
  let (width, height) = GC.getInt2Value "imagesize" gameConfig'
  gen' width
  gen' ','
  gen' height
  gen' 'L'
  labelCount <- gets nscrLabels
  gen' labelCount
  newline

  gen defineSection
  gen startSection

