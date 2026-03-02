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
  , pymoArgFailIfNotExists
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

pymoArgFailIfNotExists :: PyMO.Stmt -> Int -> Compiler T.Text
pymoArgFailIfNotExists stmt index = do
   case PyMO.stmtArgs stmt !? index of
    Just x -> return x
    Nothing -> failWithStmt stmt $ "无法访问第" ++ show (index + 1) ++ "个参数。"

type PyMOArg = T.Text

pymoArg :: PyMO.Stmt -> Int -> ([PyMOArg] -> Compiler ()) -> Compiler ()
pymoArg stmt argCount action = do
  let args = PyMO.stmtArgs stmt
      argCount' = length args
  if argCount' < argCount
    then warnWithStmt stmt $
      "需要 " ++ show argCount ++
      " 个参数，但提供了 " ++ show argCount' ++ " 个参数，忽略该命令。"
    else action args

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
        _ -> failWithStmt stmt $ "无效的条件表达式: " ++ T.unpack expr
    Nothing -> failWithStmt stmt $ "无法识别的运算符 in: " ++ T.unpack expr

goto :: ScriptId -> AllLabeledBlocks -> NextLabeledBlocks -> CommandHandler ()
goto scriptId allLabels nextLabels stmt = do
  label <- pymoArgFailIfNotExists stmt 0
  nsLabel <- findGotoNScrLabel scriptId allLabels nextLabels label stmt
  writeBody $ "goto " <> nsLabel

ifGoto :: ScriptId -> AllLabeledBlocks -> NextLabeledBlocks -> CommandHandler ()
ifGoto scriptId allLabels nextLabels stmt = do
  condition <- pymoArgFailIfNotExists stmt 0
  gotoPart <- pymoArgFailIfNotExists stmt 1
  let conditionExpr = T.strip condition
  let gotoParts = T.words (T.strip gotoPart)
  case gotoParts of
    (g:labelWords) | T.toLower g == T.pack "goto" -> do
      let targetLabel = T.unwords labelWords
      when (T.null targetLabel) $
        failWithStmt stmt $ "if命令缺少目标标签"
      nscrCondition <- parseCondition stmt conditionExpr
      nsLabel <- findGotoNScrLabel scriptId allLabels nextLabels targetLabel stmt
      writeBody $ "if " <> nscrCondition <> " goto " <> nsLabel
    _ -> failWithStmt stmt $ "if命令缺少'goto'关键字，gotoPart: " ++ T.unpack gotoPart


changeTemplate :: TB.TextBuilder -> CommandHandler ReferencedScriptName
changeTemplate nsCmd stmt = do
  targetScriptName <- pymoArgFailIfNotExists stmt 0
  (scrId, _) <- loadPyMOScript targetScriptName
  writeBody $ nsCmd <> " " <> getNScrLabel scrId 0
  return targetScriptName

change :: CommandHandler ReferencedScriptName
change = changeTemplate "goto"

call :: CommandHandler ReferencedScriptName
call = changeTemplate "gosub"

-- trivial commands

-- 背景命令（支持可选参数：渐变效果、时间、x,y坐标）
bgCommand :: CommandHandler ()
bgCommand stmt = do
  pymoArg stmt 1 $ \[bgName] -> do
    addAsset Bg bgName
    -- 简单实现：忽略可选参数，只使用文件名
    -- TODO: 从gameconfig.txt获取文件扩展名
    writeBody $ "bg \"bg/" <> TB.string (T.unpack bgName) <> ".jpg\""

-- 文本显示命令（支持可选说话人名字）
sayCommand :: CommandHandler ()
sayCommand stmt = do
  -- 参数可能为1个（只有文本）或2个（名字,文本）
  case PyMO.stmtArgs stmt of
    [textContent] -> do
      writeBody $ TB.string (T.unpack textContent)
      tailBody
    [name, text] -> do
      -- NScripter中可能需要特殊格式显示说话人名字
      writeBody $ "[" <> TB.string (T.unpack name) <> "]" <> TB.string (T.unpack text)
      tailBody
    _ -> warnWithStmt stmt "say命令参数数量不正确"
  where
    tailBody = do
      writeBody "click"
      writeBody "cl"

-- 文字显示命令（不记入对话记录）
textCommand :: CommandHandler ()
textCommand stmt = do
  -- 参数复杂：content, x1,y1,x2,y2,color,size,show_immediately
  -- 简单实现：只输出文本内容
  pymoArg stmt 1 $ \[textContent] -> do
    writeBody $ TB.string (T.unpack textContent)

-- 消除文字命令
textOffCommand :: CommandHandler ()
textOffCommand _stmt = do
  writeBody $ "cl"

-- 等待按键命令
waitkeyCommand :: CommandHandler ()
waitkeyCommand _stmt = do
  writeBody $ "click"

-- 设置章节标题命令
titleCommand :: CommandHandler ()
titleCommand stmt = do
  pymoArg stmt 1 $ \[titleContent] -> do
    -- 记录标题，但NScripter中可能需要特殊处理
    writeBody $ "; 标题: " <> TB.string (T.unpack titleContent)

-- 显示章节标题命令
titleDspCommand :: CommandHandler ()
titleDspCommand _stmt = do
  writeBody $ "; 显示标题"

-- 屏幕闪光效果命令
flashCommand :: CommandHandler ()
flashCommand stmt = do
  pymoArg stmt 2 $ \[color, time] -> do
    writeBody $ "flash " <> TB.string (T.unpack color) <> "," <> TB.string (T.unpack time)

-- 画面振动效果命令
quakeCommand :: CommandHandler ()
quakeCommand _stmt = do
  writeBody $ "quake"

-- 屏幕淡出命令
fadeOutCommand :: CommandHandler ()
fadeOutCommand stmt = do
  pymoArg stmt 2 $ \[color, time] -> do
    writeBody $ "fadeout " <> TB.string (T.unpack color) <> "," <> TB.string (T.unpack time)

-- 屏幕淡入命令
fadeInCommand :: CommandHandler ()
fadeInCommand stmt = do
  pymoArg stmt 1 $ \[time] -> do
    writeBody $ "fadein " <> TB.string (T.unpack time)

-- 更换对话框图片命令
textboxCommand :: CommandHandler ()
textboxCommand stmt = do
  pymoArg stmt 2 $ \[messageImg, nameImg] -> do
    addAsset System messageImg
    addAsset System nameImg
    writeBody $ "textbox " <> TB.string (T.unpack messageImg) <> "," <> TB.string (T.unpack nameImg)

-- 立绘振动效果命令
charaQuakeCommand :: CommandHandler ()
charaQuakeCommand stmt = do
  -- 参数：charaID1, charaID2,...
  let charaIDs = PyMO.stmtArgs stmt
  let idsStr = T.intercalate "," charaIDs
  writeBody $ "charaquake " <> TB.string (T.unpack idsStr)

-- 背景音乐命令（支持循环参数）
bgmCommand :: CommandHandler ()
bgmCommand stmt = do
  case PyMO.stmtArgs stmt of
    [] -> warnWithStmt stmt "bgm命令需要至少1个参数"
    (bgmName:_) -> do
      addAsset Bgm bgmName
      -- 检查是否有循环参数
      _ <- case PyMO.stmtArgs stmt of
        (_:loopArg:_) -> pure loopArg  -- 有第二个参数
        _ -> pure "1"  -- 默认循环
      writeBody $ "bgm \"bgm/" <> TB.string (T.unpack bgmName) <> ".mp3\""

-- 停止背景音乐命令
bgmStopCommand :: CommandHandler ()
bgmStopCommand _stmt = do
  writeBody $ "bgmstop"

-- 音效命令（支持循环参数）
seCommand :: CommandHandler ()
seCommand stmt = do
  case PyMO.stmtArgs stmt of
    [] -> warnWithStmt stmt "se命令需要至少1个参数"
    (seName:_) -> do
      addAsset Se seName
      -- 检查是否有循环参数
      _ <- case PyMO.stmtArgs stmt of
        (_:loopArg:_) -> pure loopArg  -- 有第二个参数
        _ -> pure "0"  -- 默认不循环
      writeBody $ "se \"se/" <> TB.string (T.unpack seName) <> ".wav\""

-- 停止音效命令
seStopCommand :: CommandHandler ()
seStopCommand _stmt = do
  writeBody $ "sestop"

-- 语音命令
voCommand :: CommandHandler ()
voCommand stmt = do
  pymoArg stmt 1 $ \[voiceName] -> do
    addAsset Voice voiceName
    writeBody $ "voice \"voice/" <> TB.string (T.unpack voiceName) <> ".wav\""

-- 等待命令
waitCommand :: CommandHandler ()
waitCommand stmt = do
  pymoArg stmt 1 $ \[time] -> do
    writeBody $ "wait " <> TB.string (T.unpack time)

-- 等待音效结束命令
waitSeCommand :: CommandHandler ()
waitSeCommand _stmt = do
  writeBody $ "waitse"

-- 随机数生成命令
randCommand :: CommandHandler ()
randCommand stmt = do
  pymoArg stmt 3 $ \args -> do
    let [varName, minVal, maxVal] = args
    -- 转换为NScripter变量名
    nsVar <- pymoVarToNSVar varName
    writeBody $ "rnd " <> nsVar <> "," <> TB.string (T.unpack minVal) <> "," <> TB.string (T.unpack maxVal)

-- 立绘下沉效果命令（暂时与振动效果相同）
charaDownCommand :: CommandHandler ()
charaDownCommand = charaQuakeCommand

-- 立绘上跳效果命令（暂时与振动效果相同）
charaUpCommand :: CommandHandler ()
charaUpCommand = charaQuakeCommand

trivialCommands :: HM.HashMap Command (CommandHandler ())
trivialCommands = HM.fromList
  [ ("bg", bgCommand)
  , ("say", sayCommand)
  , ("text", textCommand)
  , ("text_off", textOffCommand)
  , ("waitkey", waitkeyCommand)
  , ("title", titleCommand)
  , ("title_dsp", titleDspCommand)
  , ("flash", flashCommand)
  , ("quake", quakeCommand)
  , ("fade_out", fadeOutCommand)
  , ("fade_in", fadeInCommand)
  , ("textbox", textboxCommand)
  , ("chara_quake", charaQuakeCommand)
  , ("chara_down", charaDownCommand)
  , ("chara_up", charaUpCommand)
  , ("bgm", bgmCommand)
  , ("bgm_stop", bgmStopCommand)
  , ("se", seCommand)
  , ("se_stop", seStopCommand)
  , ("vo", voCommand)
  , ("wait", waitCommand)
  , ("wait_se", waitSeCommand)
  , ("rand", randCommand)
  ]
