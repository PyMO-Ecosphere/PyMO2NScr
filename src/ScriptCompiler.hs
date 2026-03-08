{-# LANGUAGE OverloadedStrings #-}

module ScriptCompiler (compileAllScripts) where

import Compiler
import qualified Language.PyMO.Script as PyMO
import qualified Language.PyMO.GameConfig as PyMO
import qualified Data.Text as T
import qualified TextBuilder as TB
import qualified Data.HashMap.Strict as HM
import qualified CommandCompiler as CC
import Control.Monad (forM, forM_)
import Data.Maybe (catMaybes)
import DefinesGenerator
import Control.Monad.Error.Class (MonadError(catchError, throwError))
import CommandCompiler (invalidArg, ReferencedScriptName)

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

checkGameConfig :: Compiler ReferencedScriptName
checkGameConfig = do
  gameConfig <- getCompilerInput ciPyMOGameConfig

  let gameTitle = T.filter (/= '\"') $ PyMO.getTextValue "gametitle" gameConfig
  writeDefine $ "caption " <> "\"" <> TB.text gameTitle <> "\""
  writeDefine $ "versionstr " <> "\"" <> TB.text gameTitle <> "\", \"PyMO2NScr\""

  let platform = T.unpack $ PyMO.getTextValue "platform" gameConfig
  if platform /= "s60v5" && platform /= "pygame" && platform /= "s60v3" then
    warn $ "gameconfig.txt 中存在未知的 platform 值：" ++ platform
  else pure ()

  let engineVersion = read $ T.unpack $ PyMO.getTextValue "engineversion" gameConfig
  if engineVersion > (1.2 :: Double) then
    warn $ "gameconfig.txt 的 engine version 字段大于 1.2，可能会存在无法被正常识别的 pymo 命令。"
  else pure ()

  case PyMO.getTextValue "scripttype" gameConfig of
    "pymo" -> pure ()
    x | x `elem` [ "mo1", "MO1", "mo2", "MO2" ] ->
      throwError (Nothing, "MO1 和 MO2 脚本类型必须先安装 MO2PyMO 补丁才能编译。")
    s -> throwError (Nothing, "无法识别的 scripttype 值：" ++ T.unpack s)

  let imageSize = PyMO.getInt2Value "imagesize" gameConfig
  writeDefine $ "numalias CONST_PYMO_GAMECONFIG_IMAGESIZE_X," <> TB.decimal (fst imageSize)
  writeDefine $ "numalias CONST_PYMO_GAMECONFIG_IMAGESIZE_Y," <> TB.decimal (snd imageSize)

  let nameboxOrig = PyMO.getInt2Value "nameboxorig" gameConfig
  writeDefine $ "numalias CONST_PYMO_GAMECONFIG_NAMEBOXORIG_X," <> TB.decimal (fst nameboxOrig)
  writeDefine $ "numalias CONST_PYMO_GAMECONFIG_NAMEBOXORIG_Y," <> TB.decimal (snd nameboxOrig)

  nameAlign <- case PyMO.getTextValue "namealign" gameConfig of
    "left" -> pure (0 :: Int)
    "right" -> pure 1
    "middle" -> pure 2
    s -> warn ("无法识别的 textalign 值：" ++ T.unpack s) >> pure 2

  writeDefine $ "numalias CONST_PYMO_GAMECONFIG_NAMEALIGN," <> TB.decimal nameAlign

  return $ PyMO.getTextValue "startscript" gameConfig


compileAllScripts :: Compiler ()
compileAllScripts = do
  startScript <- checkGameConfig
  forM_ pymoBootLogo compileTrivialCommand
  compileAllScripts' startScript
  generateDefines
