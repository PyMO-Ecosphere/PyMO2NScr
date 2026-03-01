{-# LANGUAGE OverloadedStrings #-}

module DefinesGenerator ( generateDefines ) where

import qualified Data.HashMap.Strict as HM
import qualified TextBuilder as TB
import qualified Language.PyMO.GameConfig as PyMO
import Compiler
import Control.Monad (forM_)
import Data.List (sort)

runtimeVariables :: Int
runtimeVariables = 10

indexed :: [a] -> [(Int, a)]
indexed = zip [0..]

defineVariables :: Compiler ()
defineVariables = do
  localVars <- genVarsId csLocalVariables runtimeVariables
  globalVars <- genVarsId csGlobalVariables $ runtimeVariables + length localVars
  forM_ (localVars ++ globalVars) $ uncurry defineVar
  return ()
  where
    defineVar :: Int -> TB.TextBuilder -> Compiler ()
    defineVar varId varName =
      writeDefine $ "numalias " <> varName <> "," <> TB.string (show varId)
    genVarsId getter startIndex = do
      vars <- indexed <$> sort <$> fmap snd <$> HM.toList <$> getCompilerState getter
      return $ map (\(varId, varName) -> (varId + startIndex, TB.text varName)) vars

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
---- | | 本地变量 + 运行时变量 < 200 -> 不生成Header
---- | | 本地变量 + 运行时变量 > 200 -> ;采用 ;$ 语法
---- | 其他分辨率 -> ;采用 ;$ 语法
---- 否则 -> 采用 ;$ 语法
---- 记得初始化所有变量

defineHeader :: Compiler ()
defineHeader = do
  let getVarsCount = \getter -> HM.size <$> getCompilerState getter
  localVars <- getVarsCount csLocalVariables
  globalVars <- getVarsCount csGlobalVariables
  let v = runtimeVariables + localVars + globalVars
  let g = runtimeVariables + localVars
  gameConfig <- getCompilerInput ciPyMOGameConfig
  let (sw, sh) = PyMO.getInt2Value "imagesize" gameConfig
  l <- getCompilerState csLabelCount
  writeHeader $
    ";$V" <> TB.decimal v <>
    "G" <> TB.decimal g
    <> "S" <> TB.decimal sw <> "," <> TB.decimal sh <>
    "L" <> TB.decimal l

generateDefines :: Compiler ()
generateDefines = defineVariables >> defineHeader
