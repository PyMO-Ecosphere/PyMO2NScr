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
  , call
  , toFullWidthChar
  , toFullWidthText) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Language.PyMO.Script as PyMO
import qualified TextBuilder as TB
import Control.Monad (when)
import Data.Char (isDigit, ord, chr)
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
type TrivialCommandHandler = PyMO.Stmt -> [PyMOArg] -> Compiler ()

-- | 将字符强制转换为全角字符
-- 半角ASCII字符 (!-~) 转换为对应的全角字符
-- 半角空格 (0x20) 转换为全角空格 (0x3000)
-- 其他字符保持不变
toFullWidthChar :: Char -> Char
toFullWidthChar c
  | c == ' '  = '\x3000'      -- 半角空格转全角空格
  | c >= '!' && c <= '~' = chr (ord c + 0xFEE0)  -- 半角ASCII转全角
  | otherwise = c

-- | 将文本中的字符强制转换为全角
toFullWidthText :: T.Text -> T.Text
toFullWidthText = T.map toFullWidthChar

getNScrLabel :: ScriptId -> LabelId -> TB.TextBuilder
getNScrLabel scriptId labelId =
  "*PYMO_" <> TB.decimal scriptId <> "_" <> TB.decimal labelId

type PyMOArg = T.Text

pymoArg :: PyMO.Stmt -> Int -> Compiler PyMOArg
pymoArg stmt index = do
   case PyMO.stmtArgs stmt !? index of
    Just x -> return x
    Nothing -> throwWithStmt stmt $ "无法访问第" ++ show (index + 1) ++ "个参数。"

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
          throwWithStmt stmt $ "未能找到跳转目标标签 " ++ T.unpack gotoTargetLabel ++ " 。"
  where isThatLabel (_, labelName, _) = labelName == gotoTargetLabel

-- 解析PyMO条件表达式
parseCondition :: PyMO.Stmt -> T.Text -> Compiler TB.TextBuilder
parseCondition stmt expr = do
  -- 根据PyMO命令表，条件表达式格式：变量 运算符 值
  -- 例如："F11=0" 或 "F11>=S1"
  -- 支持的运算符：=, >, >=, <=, !=
  let operators = ["!=", ">=", "<=", "=", ">", "<"]
      findOperator = find (\op -> op `T.isInfixOf` expr) operators

  case findOperator of
    Just op -> do
      let parts = T.splitOn op expr
      case parts of
        [leftPart, rightPart] -> do
          let leftVar = T.strip leftPart
              rightVal = T.strip rightPart
          -- 转换左操作数（变量）
          nsLeft <- pymoVarToNSVar leftVar
          -- 检查右操作数是否为变量
          nsRight <- if T.all (\c -> isDigit c || c == '-') rightVal
            then pure (TB.string $ T.unpack rightVal)  -- 常量
            else pymoVarToNSVar rightVal               -- 变量
          -- 调整运算符：PyMO使用"="，NScripter使用"=="
          let nscrOp = if op == "=" then "==" else TB.string (T.unpack op)
          pure $ nsLeft <> nscrOp <> nsRight
        _ -> throwWithStmt stmt $ "无效的条件表达式: " ++ T.unpack expr
    Nothing -> throwWithStmt stmt $ "无法识别的运算符 in: " ++ T.unpack expr

goto :: ScriptId -> AllLabeledBlocks -> NextLabeledBlocks -> CommandHandler ()
goto scriptId allLabels nextLabels stmt = do
  label <- pymoArg stmt 0
  nsLabel <- findGotoNScrLabel scriptId allLabels nextLabels label stmt
  writeBody $ "goto " <> nsLabel

ifGoto :: ScriptId -> AllLabeledBlocks -> NextLabeledBlocks -> CommandHandler ()
ifGoto scriptId allLabels nextLabels stmt = do
  condition <- pymoArg stmt 0
  gotoPart <- pymoArg stmt 1
  let conditionExpr = T.strip condition
  let gotoParts = T.words (T.strip gotoPart)
  case gotoParts of
    (g:labelWords) | T.toLower g == T.pack "goto" -> do
      let targetLabel = T.unwords labelWords
      when (T.null targetLabel) $
        throwWithStmt stmt $ "if命令缺少目标标签"
      nscrCondition <- parseCondition stmt conditionExpr
      nsLabel <- findGotoNScrLabel scriptId allLabels nextLabels targetLabel stmt
      writeBody $ "if " <> nscrCondition <> " goto " <> nsLabel
    _ -> throwWithStmt stmt $ "if命令缺少'goto'关键字，gotoPart: " ++ T.unpack gotoPart

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

trivialCommands :: HM.HashMap Command TrivialCommandHandler
trivialCommands = HM.fromList
  []
