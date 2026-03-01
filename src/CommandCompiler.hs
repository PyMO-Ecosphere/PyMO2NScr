{-# LANGUAGE OverloadedStrings #-}

module CommandCompiler
  ( LabelName
  , Block
  , LabelBlock
  , ReferencedScriptName
  , LabelId
  , AllLabeledBlocks
  , NextLabeledBlocks
  , Command
  , CommandHandler
  , getNScrLabel
  , trivialCommands
  , pymoArg
  , goto
  , ifGoto
  , change
  , call) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Language.PyMO.Script as PyMO
import qualified TextBuilder as TB
import Data.List ((!?), find)
import Compiler

-- utils
type LabelName = T.Text
type Block = [PyMO.Stmt]
type LabelBlock = (LabelId, LabelName, Block)
type ReferencedScriptName = ScriptName
type LabelId = Int
type AllLabeledBlocks = [LabelBlock]
type NextLabeledBlocks = [LabelBlock]
type Command = T.Text
type CommandHandler a = PyMO.Stmt -> Compiler a

getNScrLabel :: ScriptId -> LabelId -> TB.TextBuilder
getNScrLabel scriptId labelId =
  "*PYMO_" <> TB.string (show scriptId) <> "_" <> TB.string (show labelId)

pymoArg :: PyMO.Stmt -> Int -> Compiler T.Text
pymoArg stmt index = do
   case PyMO.stmtArgs stmt !? index of
    Just x -> return x
    Nothing -> failWithStmt stmt $ "无法访问第" ++ show (index + 1) ++ "个参数。"

-- non-trivial commands

findGotoNScrLabel ::
  ScriptId ->
  AllLabeledBlocks ->
  NextLabeledBlocks ->
  LabelName ->
  PyMO.Stmt ->
  Compiler TB.TextBuilder
findGotoNScrLabel scriptId allLabels nextLabels gotoTargetLabel stmt =
  case find isThatLabel nextLabels of
    Just (labelId, _, _) -> pure $ getNScrLabel scriptId labelId
    Nothing ->
      case find isThatLabel allLabels of
        Just (labelId, _, _) -> pure $ getNScrLabel scriptId labelId
        Nothing ->
          failWithStmt stmt $ "未能找到跳转目标标签 " ++ T.unpack gotoTargetLabel ++ " 。"
  where isThatLabel (_, labelName, _) = labelName == gotoTargetLabel

goto :: ScriptId -> AllLabeledBlocks -> NextLabeledBlocks -> CommandHandler ()
goto scriptId allLabels nextLabels stmt = do
  label <- pymoArg stmt 0
  nsLabel <- findGotoNScrLabel scriptId allLabels nextLabels label stmt
  writeBody $ "goto " <> nsLabel

ifGoto :: ScriptId -> AllLabeledBlocks -> NextLabeledBlocks -> CommandHandler ()
ifGoto _ _ _ _ = undefined -- AI 帮我实现这里

changeTemplate :: TB.TextBuilder -> CommandHandler ReferencedScriptName
changeTemplate nsCmd stmt = do
  targetScriptName <- pymoArg stmt 0
  (scrId, _) <- loadPyMOScript targetScriptName
  writeBody $ nsCmd <> " " <> getNScrLabel scrId 0
  return targetScriptName

change :: CommandHandler ReferencedScriptName
change = changeTemplate "goto"

call :: CommandHandler ReferencedScriptName
call = changeTemplate "gosub"

-- trivial commands

trivialCommands :: HM.HashMap Command (CommandHandler ())
trivialCommands = HM.fromList []
