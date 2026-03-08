{-# LANGUAGE OverloadedStrings #-}

module DefinesGenerator ( generateDefines ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified TextBuilder as TB
import qualified Data.Text as T
import qualified Language.PyMO.GameConfig as PyMO
import Compiler
import Control.Monad (forM_)
import Data.List (sort)

runtimeVariables :: Int
runtimeVariables = 32

-- runtime vars
-- 0~16 子程序用的临时变量
-- 24~31 游戏状态变量

-- | runtime vars | local vars | gap | global vars | padding |
--                |            |     |             |
--                1            2    3             4

data NSVarLayout = NSVarLayout
  { nslLocalVarEnd :: Int   -- 2
  , nslGlobalVarBegin :: Int -- 3
  , nslGlobalVarEnd :: Int  -- 4
  }

defineVariables :: Compiler NSVarLayout
defineVariables = do
  lVars <- getVars csLocalVariables
  gVarsPyMO <- getVars csGlobalVariables
  generateBootInitialization lVars gVarsPyMO
  gVarsRuntime <- HS.toList <$> getCompilerState csRuntimeGlobalVariables
  let gVars = gVarsRuntime ++ gVarsPyMO
  let lVarBegin = runtimeVariables
      lVarEnd = runtimeVariables + length lVars
      gVarBegin = max 200 lVarEnd
      gVarEnd = gVarBegin + length gVars
      lVars' = zip [lVarBegin .. lVarEnd] $ map TB.text lVars
      gVars' = zip [gVarBegin .. gVarEnd] $ map TB.text gVars
  forM_ (lVars' ++ gVars') $ uncurry defineVar
  return $ NSVarLayout lVarEnd gVarBegin gVarEnd
  where
    defineVar :: Int -> TB.TextBuilder -> Compiler ()
    defineVar varId varName =
      writeDefine $ "numalias " <> varName <> "," <> TB.string (show varId)
    getVars getter =
      sort <$> fmap snd <$> HM.toList <$> getCompilerState getter

    generateBootInitialization :: [T.Text] -> [T.Text] -> Compiler ()
    generateBootInitialization localVars globalPyMOVars = do
      defineRuntimeGlobalVariables "RTG_INIT"
      forM_ localVars $ \varName ->
        writeBoot $ "mov %" <> TB.text varName <> ",0"
      writeBoot "if $RTG_INIT=\"OK\" goto *skip_global_init"
      forM_ globalPyMOVars $ \varName ->
        writeBoot $ "mov %" <> TB.text varName <> ",0"
      writeBoot "mov $RTG_INIT,\"OK\""
      writeBoot "*skip_global_init"

defineComplexHeader :: Int -> Int -> (Int, Int) -> Int -> Compiler ()
defineComplexHeader v g (sw, sh) l = do
  writeHeader $
    ";$V" <> TB.decimal v <>
    "G" <> TB.decimal g
    <> "S" <> TB.decimal sw <> "," <> TB.decimal sh <>
    "L" <> TB.decimal l

defineSimpleHeader :: Maybe Int -> Maybe Int -> Compiler ()
defineSimpleHeader Nothing Nothing = pure ()
defineSimpleHeader (Just mode) Nothing = writeHeader $ ";mode" <> TB.decimal mode
defineSimpleHeader Nothing (Just value) = writeHeader $ ";value" <> TB.decimal value
defineSimpleHeader (Just mode) (Just value) =
  writeHeader $ ";mode" <> TB.decimal mode <> ",value" <> TB.decimal value

-- 注意：旧版ONS中不支持这种Header，需要考虑旧版兼容性，优先使用 mode语法，而不是使用 Header
-- Header还影响全局变量边界的问题
-- 应该先生成变量并返回变量生成报告，再根据变量生成报告生成 Header
--
-- Header 决策模式
---- Label数少于10000个且最大变量编号小于4095
---- | 分辨率是320*240
---- | | 本地变量 + 运行时变量 < 200 -> ;mode320
---- | | 本地变量 + 运行时变量 > 200 -> ;mode320,value语法
---- | 分辨率是400*300
---- | | 本地变量 + 运行时变量 < 200 -> ;mode400
---- | | 本地变量 + 运行时变量 > 200 -> ;mode400,value语法
---- | 分辨率是640*480
---- | | 本地变量 + 运行时变量 < 200 -> 不生成Header
---- | | 本地变量 + 运行时变量 > 200 -> ;采用 ;value 语法
---- | 分辨率是800*600
---- | | 本地变量 + 运行时变量 < 200 -> ;mode800
---- | | 本地变量 + 运行时变量 > 200 -> ;采用 ;value 语法
---- | 其他分辨率 -> ;采用 ;$ 语法
---- 否则 -> 采用 ;$ 语法
---- 记得初始化所有变量

defineHeader :: NSVarLayout -> Compiler ()
defineHeader layout = do
  let v = nslGlobalVarEnd layout
      g = nslGlobalVarBegin layout
  s <- PyMO.getInt2Value "imagesize" <$> getCompilerInput ciPyMOGameConfig
  l <- getCompilerState csLabelCount

  if l < 10000 && v < 4096 then
    case (s, nslLocalVarEnd layout) of
      ((320, 240), localVarEnd) | localVarEnd <= 200 ->
        defineSimpleHeader (Just 320) Nothing
      ((320, 240), _) ->
        defineSimpleHeader (Just 320) (Just g)
      ((400, 300), localVarEnd) | localVarEnd <= 200 ->
        defineSimpleHeader (Just 400) Nothing
      ((400, 300), _) ->
        defineSimpleHeader (Just 400) (Just g)
      ((640, 480), localVarEnd) | localVarEnd <= 200 ->
        defineSimpleHeader Nothing Nothing
      ((640, 480), _) ->
        defineSimpleHeader Nothing (Just g)
      ((800, 600), localVarEnd) | localVarEnd <= 200 ->
        defineSimpleHeader (Just 800) Nothing
      ((800, 600), _) ->
        defineSimpleHeader (Just 800) (Just g)
      _ -> defineComplexHeader v g s l
  else
    defineComplexHeader v g s l

generateDefines :: Compiler ()
generateDefines = do
  varLayout <- defineVariables
  defineHeader varLayout
