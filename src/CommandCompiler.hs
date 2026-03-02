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
  , invalidArg
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
import Control.Monad (when, forM_)
import Data.Char (isDigit, ord, chr)
import Data.List (find)
import Compiler
import qualified Language.PyMO.GameConfig as PyMO

-- utils
type LabelName = T.Text
type Block = [PyMO.Stmt]
type LabelBlock = (LabelId, LabelName, Block)
type ReferencedScriptName = ScriptName
type LabelId = Int
type AllLabeledBlocks = [LabelBlock]
type NextLabeledBlocks = [LabelBlock]
type Command = T.Text
type PyMOArg = T.Text
type CommandHandler a = PyMO.Stmt -> [PyMOArg] -> Compiler a

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

-- | 将文本用双引号包裹
quoted :: TB.TextBuilder -> TB.TextBuilder
quoted t = TB.string "\"" <> t <> TB.string "\""

-- | 获取资源文件的完整路径（带目录和扩展名）
getAssetPath :: T.Text -> T.Text -> Compiler TB.TextBuilder
getAssetPath assetType filename = do
  gameConfig <- getCompilerInput ciPyMOGameConfig
  -- 获取扩展名配置键，如 "bgformat", "bgmformat" 等
  let formatKey = assetType <> "format"
      ext = PyMO.getTextValue formatKey gameConfig
  -- 构建路径：资源类型目录 + 文件名 + 扩展名
  pure $ quoted $ TB.text assetType <> TB.string "/" <> TB.text filename <> TB.text ext

-- | 将文本用双引号包裹并进行全角转换
quotedFullWidthText :: T.Text -> TB.TextBuilder
quotedFullWidthText t = quoted $ TB.text (toFullWidthText t)

getNScrLabel :: ScriptId -> LabelId -> TB.TextBuilder
getNScrLabel scriptId labelId =
  "*PYMO_" <> TB.decimal scriptId <> "_" <> TB.decimal labelId

invalidArg :: PyMO.Stmt -> Compiler a
invalidArg stmt =
  throwWithStmt stmt $
    "无法为命令" ++
    T.unpack (PyMO.stmtCommand stmt) ++
    "匹配" ++ show (length $ PyMO.stmtArgs stmt)  ++ "个参数。"

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
goto scriptId allLabels nextLabels stmt [label] = do
  nsLabel <- findGotoNScrLabel scriptId allLabels nextLabels label stmt
  writeBody $ "goto " <> nsLabel
goto _ _ _ stmt _ = invalidArg stmt

ifGoto :: ScriptId -> AllLabeledBlocks -> NextLabeledBlocks -> CommandHandler ()
ifGoto scriptId allLabels nextLabels stmt [condition, gotoPart] = do
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
ifGoto _ _ _ stmt _ = invalidArg stmt

changeTemplate :: TB.TextBuilder -> CommandHandler ReferencedScriptName
changeTemplate nsCmd _ [targetScriptName] = do
  (scrId, _) <- loadPyMOScript targetScriptName
  writeBody $ nsCmd <> " " <> getNScrLabel scrId 0
  return targetScriptName
changeTemplate _ stmt _ = invalidArg stmt

change :: CommandHandler ReferencedScriptName
change = changeTemplate "goto"

call :: CommandHandler ReferencedScriptName
call = changeTemplate "gosub"

-- trivial commands

textOff :: CommandHandler ()
textOff stmt [] = writeBody "cl"
textOff stmt _ = invalidArg stmt

waitkey :: CommandHandler ()
waitkey stmt [] = writeBody "click"
waitkey stmt _ = invalidArg stmt

bgmStop :: CommandHandler ()
bgmStop stmt [] = writeBody "stop"
bgmStop stmt _ = invalidArg stmt

seStop :: CommandHandler ()
seStop stmt [] = writeBody "stopse"
seStop stmt _ = invalidArg stmt

ret :: CommandHandler ()
ret stmt [] = writeBody "return"
ret stmt _ = invalidArg stmt

quake :: CommandHandler ()
quake stmt [] = writeBody "quake"
quake stmt _ = invalidArg stmt

fadeIn :: CommandHandler ()
fadeIn stmt [time] = writeBody $ "fadein " <> TB.text time
fadeIn stmt _ = invalidArg stmt

fadeOut :: CommandHandler ()
fadeOut stmt [color, time] = writeBody $ "fadeout " <> TB.text color <> "," <> TB.text time
fadeOut stmt _ = invalidArg stmt

flash :: CommandHandler ()
flash stmt [color, time] = writeBody $ "flash " <> TB.text color <> "," <> TB.text time
flash stmt _ = invalidArg stmt

wait :: CommandHandler ()
wait stmt [time] = writeBody $ "wait " <> TB.text time
wait stmt _ = invalidArg stmt

waitSe :: CommandHandler ()
waitSe stmt [] = writeBody "waitsound"
waitSe stmt _ = invalidArg stmt

bgm :: CommandHandler ()
bgm stmt [filename] = do
  path <- getAssetPath "bgm" filename
  writeBody $ "bgm " <> path
bgm stmt [filename, isloop] = do
  path <- getAssetPath "bgm" filename
  writeBody $ "bgm " <> path <> "," <> TB.text isloop
bgm stmt _ = invalidArg stmt

se :: CommandHandler ()
se stmt [filename] = do
  path <- getAssetPath "se" filename
  writeBody $ "dwave 1," <> path
se stmt [filename, isloop] = do
  path <- getAssetPath "se" filename
  if isloop == "1"
    then writeBody $ "dwaveloop 1," <> path
    else writeBody $ "dwave 1," <> path
se stmt _ = invalidArg stmt

vo :: CommandHandler ()
vo stmt [filename] = do
  path <- getAssetPath "voice" filename
  writeBody $ "dwave 2," <> path
vo stmt _ = invalidArg stmt

movie :: CommandHandler ()
movie stmt [filename] = writeBody $ "movie " <> TB.text filename
movie stmt _ = invalidArg stmt

config :: CommandHandler ()
config stmt [] = writeBody "config"
config stmt _ = invalidArg stmt

music :: CommandHandler ()
music stmt [] = writeBody "music"
music stmt _ = invalidArg stmt

album :: CommandHandler ()
album stmt [] = writeBody "album"
album stmt [albumList] = writeBody $ "album " <> TB.text albumList
album stmt _ = invalidArg stmt

load :: CommandHandler ()
load stmt [] = writeBody "load"
load stmt [saveNum] = writeBody $ "load " <> TB.text saveNum
load stmt _ = invalidArg stmt

date :: CommandHandler ()
date stmt [dateBg, x, y, color] = do
  -- 简单实现：显示日期背景，实际日期显示需要更复杂的处理
  path <- getAssetPath "bg" dateBg
  writeBody $ "print " <> path <> "," <> TB.text x <> "," <> TB.text y <> "," <> TB.text color <> ",\"日期显示功能待完善\""
date stmt _ = invalidArg stmt

text :: CommandHandler ()
text stmt args
  | length args == 8 = do
    let [content, x1, y1, x2, y2, color, size, showImmediately] = args
    -- 简单实现：使用print命令，忽略一些样式参数
    writeBody $ "print " <> quotedFullWidthText content <> "," <> TB.text x1 <> "," <> TB.text y1
  | otherwise = invalidArg stmt

title :: CommandHandler ()
title stmt [content] = writeBody $ "savetitle " <> quotedFullWidthText content
title stmt _ = invalidArg stmt

titleDsp :: CommandHandler ()
titleDsp stmt [] = writeBody "print \"标题显示功能待完善\""
titleDsp stmt _ = invalidArg stmt

set :: CommandHandler ()
set stmt [varName, val] = do
  nsVar <- pymoVarToNSVar varName
  nsVal <- if T.all (\c -> isDigit c || c == '-') val
    then pure $ TB.text val
    else ("%" <>) <$> pymoVarToNSVar val
  writeBody $ "mov %" <> nsVar <> "," <> nsVal
set stmt _ = invalidArg stmt

add :: CommandHandler ()
add stmt [varName, val] = do
  nsVar <- pymoVarToNSVar varName
  nsVal <- if T.all (\c -> isDigit c || c == '-') val
    then pure $ TB.text val
    else ("%" <>) <$> pymoVarToNSVar val
  writeBody $ "add %" <> nsVar <> "," <> nsVal
add stmt _ = invalidArg stmt

sub :: CommandHandler ()
sub stmt [varName, val] = do
  nsVar <- pymoVarToNSVar varName
  nsVal <- if T.all (\c -> isDigit c || c == '-') val
    then pure $ TB.text val
    else ("%" <>) <$> pymoVarToNSVar val
  writeBody $ "sub %" <> nsVar <> "," <> nsVal
sub stmt _ = invalidArg stmt

rand :: CommandHandler ()
rand stmt [varName, minVal, maxVal] = do
  nsVar <- pymoVarToNSVar varName
  nsMin <- if T.all (\c -> isDigit c || c == '-') minVal
    then pure $ TB.text minVal
    else ("%" <>) <$> pymoVarToNSVar minVal
  nsMax <- if T.all (\c -> isDigit c || c == '-') maxVal
    then pure $ TB.text maxVal
    else ("%" <>) <$> pymoVarToNSVar maxVal
  writeBody $ "rnd %" <> nsVar <> "," <> nsMin <> "," <> nsMax
rand stmt _ = invalidArg stmt

textbox :: CommandHandler ()
textbox stmt [message, name] = writeBody $ "; textbox command not implemented: " <> TB.text message <> "," <> TB.text name
textbox stmt _ = invalidArg stmt

bg :: CommandHandler ()
bg stmt args = case args of
  [filename] -> do
    path <- getAssetPath "bg" filename
    writeBody $ "bg " <> path
  [filename, transition, time] -> do
    path <- getAssetPath "bg" filename
    writeBody $ "bg " <> path <> "," <> TB.text transition <> "," <> TB.text time
  [filename, transition, time, x, y] -> do
    path <- getAssetPath "bg" filename
    writeBody $ "bg " <> path <> "," <> TB.text transition <> "," <> TB.text time <> "," <> TB.text x <> "," <> TB.text y
  _ -> invalidArg stmt

scroll :: CommandHandler ()
scroll stmt [filename, startx, starty, endx, endy, time] = do
  path <- getAssetPath "bg" filename
  writeBody $ "scroll " <> path <> "," <> TB.text startx <> "," <> TB.text starty <> "," <> TB.text endx <> "," <> TB.text endy <> "," <> TB.text time
scroll stmt _ = invalidArg stmt

sel :: CommandHandler ()
sel stmt args = case args of
  [choiceNum] -> writeBody $ "select " <> TB.text choiceNum
  [choiceNum, hintPic] -> writeBody $ "select " <> TB.text choiceNum <> "," <> TB.text hintPic
  _ -> invalidArg stmt

selectText :: CommandHandler ()
selectText stmt args
  | length args >= 9 = do
    let choiceNum = head args
    let choiceTexts = take (read (T.unpack choiceNum) :: Int) (tail args)
    let restArgs = drop (1 + read (T.unpack choiceNum) :: Int) args
    case restArgs of
      [x1, y1, x2, y2, color, initPos] ->
        writeBody $ "select " <> TB.text choiceNum <> "," <> TB.text x1 <> "," <> TB.text y1 <> "," <> TB.text x2 <> "," <> TB.text y2 <> "," <> TB.text color <> "," <> TB.text initPos
      _ -> invalidArg stmt
  | otherwise = invalidArg stmt

say :: CommandHandler ()
say stmt args = case args of
  [name, content] -> writeBody $ TB.text (toFullWidthText (name <> "，" <> content)) <> "@"
  [content] -> writeBody $ TB.text (toFullWidthText content) <> "@"
  _ -> invalidArg stmt

chara :: CommandHandler ()
chara stmt args
  | length args >= 5 && (length args - 1) `mod` 4 == 0 = do
    let time = last args
        charaArgs = init args
        numChara = (length charaArgs) `div` 4
    forM_ [0..numChara-1] $ \i -> do
      let idx = i * 4
          charaID = charaArgs !! idx
          filename = charaArgs !! (idx + 1)
          position = charaArgs !! (idx + 2)
          layer = charaArgs !! (idx + 3)
      if filename == "NULL"
        then writeBody $ "csp " <> TB.text charaID
        else do
          path <- getAssetPath "chara" filename
          writeBody $ "lsp " <> TB.text charaID <> "," <> path <> "," <> TB.text position <> ",0"
    -- 忽略time参数，因为lsp没有淡入时间参数
    pure ()
  | otherwise = invalidArg stmt

charaCls :: CommandHandler ()
charaCls stmt args = case args of
  ["a"] -> writeBody "csp -1"
  ["a", _time] -> writeBody "csp -1"  -- 忽略time参数
  [charaID] -> writeBody $ "csp " <> TB.text charaID
  [charaID, _time] -> writeBody $ "csp " <> TB.text charaID
  _ -> invalidArg stmt

charaPos :: CommandHandler ()
charaPos stmt args = case args of
  [charaID, newX, newY, coordMode] ->
    writeBody $ "amsp " <> TB.text charaID <> "," <> TB.text newX <> "," <> TB.text newY <> ",0"
  _ -> invalidArg stmt

charaY :: CommandHandler ()
charaY stmt args
  | length args >= 7 && (length args - 2) `mod` 5 == 0 = do
    let coordMode = head args
        time = last args
        charaArgs = tail (init args)  -- 去掉coordMode和time
        numChara = (length charaArgs) `div` 5
    forM_ [0..numChara-1] $ \i -> do
      let idx = i * 5
          charaID = charaArgs !! idx
          filename = charaArgs !! (idx + 1)
          x = charaArgs !! (idx + 2)
          y = charaArgs !! (idx + 3)
          layer = charaArgs !! (idx + 4)
      if filename == "NULL"
        then writeBody $ "csp " <> TB.text charaID
        else writeBody $ "lsp " <> TB.text charaID <> "," <> TB.text filename <> "," <> TB.text x <> "," <> TB.text y
    -- 忽略coordMode和time参数
    pure ()
  | otherwise = invalidArg stmt

charaQuake :: CommandHandler ()
charaQuake stmt args
  | not (null args) = do
    -- 立绘振动效果，NScripter中无直接对应命令，输出注释
    writeBody $ "; chara_quake " <> TB.string (show args)
    -- 可以使用quake命令作为替代，但会影响整个屏幕
    writeBody "quake"
  | otherwise = invalidArg stmt

charaDown :: CommandHandler ()
charaDown stmt args
  | not (null args) = do
    -- 立绘下沉效果，NScripter中无直接对应命令，输出注释
    writeBody $ "; chara_down " <> TB.string (show args)
  | otherwise = invalidArg stmt

charaUp :: CommandHandler ()
charaUp stmt args
  | not (null args) = do
    -- 立绘上跳效果，NScripter中无直接对应命令，输出注释
    writeBody $ "; chara_up " <> TB.string (show args)
  | otherwise = invalidArg stmt

charaScroll :: CommandHandler ()
charaScroll stmt args = case length args of
  5 -> -- 简化版本: coord_mode, charaID, endx, endy, time
    let [coordMode, charaID, endx, endy, time] = args
    in writeBody $ "; chara_scroll simplified " <> TB.string (show args)
  10 -> -- 完整版本: coord_mode, charaID, filename, startx, starty, endx, endy, beginalpha, layer, time
    let [coordMode, charaID, filename, startx, starty, endx, endy, beginalpha, layer, time] = args
    in writeBody $ "; chara_scroll full " <> TB.string (show args)
  _ -> invalidArg stmt

animeOn :: CommandHandler ()
animeOn stmt args = case args of
  [num, filename, x, y, interval, isloop] ->
    writeBody $ "; anime_on " <> TB.string (show args)
  _ -> invalidArg stmt

animeOff :: CommandHandler ()
animeOff stmt args = case args of
  [filename] -> writeBody $ "; anime_off " <> TB.text filename
  _ -> invalidArg stmt

selectVar :: CommandHandler ()
selectVar stmt args
  | length args >= 7 = do  -- 至少choice_num=1时有7个参数
    let choiceNumText = head args
        choiceNum = read (T.unpack choiceNumText) :: Int
        totalExpected = 1 + 2*choiceNum + 6
    if length args == totalExpected
      then writeBody $ "; select_var " <> TB.string (show args)
      else invalidArg stmt
  | otherwise = invalidArg stmt

selectImg :: CommandHandler ()
selectImg stmt args
  | length args >= 5 = do  -- 至少choice_num=1时有5个参数：choice_num, filename, x1, y1, var1, init_position
    let choiceNumText = head args
        choiceNum = read (T.unpack choiceNumText) :: Int
        totalExpected = 1 + 3*choiceNum + 1  -- choice_num + 3*choice_num + init_position
    if length args == totalExpected
      then writeBody $ "; select_img " <> TB.string (show args)
      else invalidArg stmt
  | otherwise = invalidArg stmt

selectImgs :: CommandHandler ()
selectImgs stmt args
  | length args >= 6 = do  -- 至少choice_num=1时有6个参数：choice_num, filename1, x1, y1, var1, init_position
    let choiceNumText = head args
        choiceNum = read (T.unpack choiceNumText) :: Int
        totalExpected = 1 + 4*choiceNum + 1  -- choice_num + 4*choice_num + init_position
    if length args == totalExpected
      then writeBody $ "; select_imgs " <> TB.string (show args)
      else invalidArg stmt
  | otherwise = invalidArg stmt

charaAnime :: CommandHandler ()
charaAnime stmt args
  | length args >= 3 && (length args - 3) `mod` 2 == 0 = do
    let charaID = head args
        period = args !! 1
        loopNum = args !! 2
        offsets = drop 3 args
    writeBody $ "; chara_anime " <> TB.string (show args)
  | otherwise = invalidArg stmt

trivialCommands :: HM.HashMap Command (CommandHandler ())
trivialCommands = HM.fromList
  [ ("text_off", textOff)
  , ("waitkey", waitkey)
  , ("bgm_stop", bgmStop)
  , ("se_stop", seStop)
  , ("ret", ret)
  , ("quake", quake)
  , ("fade_in", fadeIn)
  , ("fade_out", fadeOut)
  , ("flash", flash)
  , ("wait", wait)
  , ("wait_se", waitSe)
  , ("set", set)
  , ("add", add)
  , ("sub", sub)
  , ("rand", rand)
  , ("say", say)
  , ("bgm", bgm)
  , ("se", se)
  , ("vo", vo)
  , ("movie", movie)
  , ("config", config)
  , ("music", music)
  , ("album", album)
  , ("load", load)
  , ("date", date)
  , ("text", text)
  , ("title", title)
  , ("title_dsp", titleDsp)
  , ("textbox", textbox)
  , ("bg", bg)
  , ("scroll", scroll)
  , ("sel", sel)
  , ("chara", chara)
  , ("chara_cls", charaCls)
  , ("chara_pos", charaPos)
  , ("chara_y", charaY)
  , ("chara_quake", charaQuake)
  , ("chara_down", charaDown)
  , ("chara_up", charaUp)
  , ("chara_scroll", charaScroll)
  , ("anime_on", animeOn)
  , ("anime_off", animeOff)
  , ("chara_anime", charaAnime)
  , ("select_var", selectVar)
  , ("select_img", selectImg)
  , ("select_imgs", selectImgs)
  , ("select_text", selectText) ]
