{-# LANGUAGE OverloadedStrings #-}

module ScriptCompiler (compileAllScripts) where

import Compiler
import qualified Language.PyMO.Script as PyMO
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified CommandCompiler as CC
import Control.Monad (forM, forM_)
import Data.Maybe (catMaybes)
import DefinesGenerator
import Control.Monad.Error.Class (MonadError(catchError))
import CommandCompiler (invalidArg)

collectBlockUntilNextLabel ::
  [PyMO.Stmt] -> Compiler ([PyMO.Stmt], Maybe (CC.LabelName, [PyMO.Stmt]))
collectBlockUntilNextLabel [] = return ([], Nothing)
collectBlockUntilNextLabel (x : xs)
  | PyMO.stmtCommand x == "label" =
    case PyMO.stmtArgs x of
      [labelName] -> return ([], Just (labelName, xs))
      _ -> invalidArg x
  | otherwise = do
    (xs', nextLabelName) <- collectBlockUntilNextLabel xs
    return (x : xs', nextLabelName)

splitLabels :: Int -> PyMO.Script -> Compiler (CC.Block, [CC.LabelBlock])
splitLabels currentLabelId script = do
  (currentBlock, nextLabels) <- collectBlockUntilNextLabel script
  case nextLabels of
    Nothing -> return (currentBlock, [])
    Just (nextLabelName, restStmt) -> do
      let nextLabelId = currentLabelId + 1
      (nextBlock, nextLabeledBlocks) <- splitLabels nextLabelId restStmt
      return (currentBlock, (nextLabelId, nextLabelName, nextBlock) : nextLabeledBlocks)

compileTrivialCommand :: PyMO.Stmt -> Compiler ()
compileTrivialCommand stmt =
  case HM.lookup (PyMO.stmtCommand stmt) CC.trivialCommands of
    Nothing -> do
      warnWithStmt stmt $ "无法处理命令 " ++ (T.unpack $ PyMO.stmtCommand stmt) ++ " ，忽略该命令。"
    Just x -> do
      catchError (x stmt (PyMO.stmtArgs stmt)) $ \(stmt', msg) ->
        case stmt' of
          Nothing -> warn msg
          Just stmt'' -> warnWithStmt stmt'' msg

compileCommand ::
  ScriptId ->
  CC.AllLabeledBlocks ->
  CC.NextLabeledBlocks ->
  PyMO.Stmt ->
  Compiler (Maybe CC.ReferencedScriptName)
compileCommand scriptId allLabeledBlocks nextLabeledBlocks stmt =
  case PyMO.stmtCommand stmt of
    "goto" -> do
      CC.goto scriptId allLabeledBlocks nextLabeledBlocks stmt (PyMO.stmtArgs stmt)
      pure Nothing
    "if" -> do
      CC.ifGoto scriptId allLabeledBlocks nextLabeledBlocks stmt (PyMO.stmtArgs stmt)
      pure Nothing
    "change" -> Just <$> CC.change stmt (PyMO.stmtArgs stmt)
    "call" -> Just <$> CC.call stmt (PyMO.stmtArgs stmt)
    _ -> compileTrivialCommand stmt >> pure Nothing

compileBlock ::
  ScriptId ->
  CC.AllLabeledBlocks ->
  CC.NextLabeledBlocks ->
  CC.LabelId ->
  CC.Block ->
  Compiler [CC.ReferencedScriptName]
compileBlock scriptId allLabeledBlocks nextBlocks curLabelId block = do
  updateCompilerState $ \x -> x { csLabelCount = csLabelCount x + 1 }
  writeBody $ CC.getNScrLabel scriptId curLabelId
  catMaybes <$> (forM block $ compileCommand scriptId allLabeledBlocks nextBlocks)

compileLabeledBlocks ::
  ScriptId ->
  CC.AllLabeledBlocks ->
  CC.NextLabeledBlocks ->
  Compiler [CC.ReferencedScriptName]
compileLabeledBlocks _ _ [] = pure []
compileLabeledBlocks scriptId allLabeledBlocks ((labelId, _, block) : xs) = do
  refScripts <- compileBlock scriptId allLabeledBlocks xs labelId block
  nextBlocksRefScripts <- compileLabeledBlocks scriptId allLabeledBlocks xs
  return $ refScripts ++ nextBlocksRefScripts

compileScript :: ScriptName -> Compiler [CC.ReferencedScriptName]
compileScript scriptName = do
  compiled <- isScriptCompiled scriptName
  if not compiled then do
    logInfo $ "编译 " ++ T.unpack scriptName ++ " ..."
    (scriptId, script) <- loadPyMOScript scriptName
    (headBlock, labeledBlocks) <- splitLabels 0 script
    headBlockRefs <- compileBlock scriptId labeledBlocks labeledBlocks 0 headBlock
    labeledBlocksRefs <- compileLabeledBlocks scriptId labeledBlocks labeledBlocks
    writeBody "end"
    markAsCompiled scriptName
    return $ headBlockRefs ++ labeledBlocksRefs
  else
    return []

compileAllScripts' :: ScriptName -> Compiler ()
compileAllScripts' startScriptName = do
  refs <- compileScript startScriptName
  forM_ refs compileAllScripts'

pymoBootLogo :: PyMO.Script
pymoBootLogo = PyMO.parsePyMOScript "<pymo-bootloader>" $ T.unlines
  [
      "#textbox message,name",
      "#wait 50",
      "#bg logo1",
      "#wait 300",
      "#bg logo2",
      "#wait 300"
  ]


compileAllScripts :: ScriptName -> Compiler ()
compileAllScripts startScriptName = do
  forM_ pymoBootLogo compileTrivialCommand
  compileAllScripts' startScriptName
  generateDefines
