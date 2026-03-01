{-# LANGUAGE OverloadedStrings #-}

module ScriptCompiler (test) where

import Compiler
import qualified Language.PyMO.Script as PyMO
import qualified Data.Text as T
import Control.Monad (forM_)

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
