{-# LANGUAGE OverloadedStrings #-}

module ScriptCompiler (test) where

import Compiler
import qualified Language.PyMO.Script as PyMO
import qualified Data.Text as T
import Control.Monad (forM_, forM)
import Data.Maybe (catMaybes)
import TextBuilder as TB

type LabelName = T.Text
type Block = [PyMO.Stmt]
type LabelId = Int
type LabelBlock = (LabelId, LabelName, Block)

collectBlockUntilNextLabel ::
  [PyMO.Stmt] -> Compiler ([PyMO.Stmt], Maybe (LabelName, [PyMO.Stmt]))
collectBlockUntilNextLabel [] = return ([], Nothing)
collectBlockUntilNextLabel (x : xs)
  -- todo: 这里可能有越界访问，需要后续优化为安全的访问方式
  | PyMO.stmtCommand x == "label" = return ([], Just (head $ PyMO.stmtArgs x, xs))
  | otherwise = do
    (xs', nextLabelName) <- collectBlockUntilNextLabel xs
    return (x : xs', nextLabelName)

splitLabels :: Int -> PyMO.Script -> Compiler (Block, [LabelBlock])
splitLabels currentLabelId script = do
  (currentBlock, nextLabels) <- collectBlockUntilNextLabel script
  case nextLabels of
    Nothing -> return (currentBlock, [])
    Just (nextLabelName, restStmt) -> do
      let nextLabelId = currentLabelId + 1
      (nextBlock, nextLabeledBlocks) <- splitLabels nextLabelId restStmt
      return (currentBlock, (nextLabelId, nextLabelName, nextBlock) : nextLabeledBlocks)

type ReferencedScriptName = ScriptName
type AllLabeledBlocks = [LabelBlock]
type NextLabeledBlocks = [LabelBlock]

getNScrLabel :: ScriptId -> LabelId -> TB.TextBuilder
getNScrLabel scriptId labelId =
  "*PYMO_" <> TB.string (show scriptId) <> "_" <> TB.string (show labelId)

compileCommand ::
  ScriptId ->
  AllLabeledBlocks ->
  NextLabeledBlocks ->
  PyMO.Stmt ->
  Compiler (Maybe ReferencedScriptName)
compileCommand = undefined -- todo

compileBlock ::
  ScriptId ->
  AllLabeledBlocks ->
  NextLabeledBlocks ->
  LabelId ->
  Block ->
  Compiler [ReferencedScriptName]
compileBlock scriptId allLabeledBlocks nextBlocks curLabelId block = do
  -- TODO:写入 label
  catMaybes <$> (forM block $ compileCommand scriptId allLabeledBlocks nextBlocks)


compileLabeledBlocks ::
  ScriptId ->
  AllLabeledBlocks ->
  NextLabeledBlocks ->
  Compiler [ReferencedScriptName]
compileLabeledBlocks _ _ [] = pure []
compileLabeledBlocks scriptId allLabeledBlocks ((labelId, _, block) : xs) = do
  refScripts <- compileBlock scriptId allLabeledBlocks xs labelId block
  nextBlocksRefScripts <- compileLabeledBlocks scriptId allLabeledBlocks xs
  return $ refScripts ++ nextBlocksRefScripts

compileScript :: ScriptName -> Compiler [ReferencedScriptName]
compileScript scriptName = do
  compiled <- isScriptCompiled scriptName
  if not compiled then do
    logInfo $ "Compiling " ++ T.unpack scriptName ++ "..."
    (scriptId, script) <- loadPyMOScript scriptName
    (headBlock, labeledBlocks) <- splitLabels 0 script
    headBlockRefs <- compileBlock scriptId labeledBlocks labeledBlocks 0 headBlock
    labeledBlocksRefs <- compileLabeledBlocks scriptId labeledBlocks labeledBlocks
    markAsCompiled scriptName
    return $ headBlockRefs ++ labeledBlocksRefs
  else
    return []

showInfo :: ScriptName -> Compiler ()
showInfo scrName = do
  (scrId, scr) <- loadPyMOScript scrName
  (headBlock, labeledBlocks) <- splitLabels 0 scr
  logInfo ""
  logInfo $ "====" ++ T.unpack scrName ++ "("  ++ show scrId ++ ")"  ++ ":" ++ "===="

  logInfo "== Head Block =="
  forM_ headBlock $ \x -> logInfo $ T.unpack (PyMO.stmtCommand x)

  forM_ labeledBlocks $ \(labelId, labelName, block) -> do
    logInfo $ "== Label " ++ T.unpack labelName ++ "(" ++ show labelId ++ ")" ++ " =="
    forM_ block $ \x -> logInfo $ T.unpack (PyMO.stmtCommand x)

test :: Compiler ()
test = do
  showInfo "start"
  showInfo "test"
