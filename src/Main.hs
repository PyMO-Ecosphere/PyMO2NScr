module Main (main) where

import Data.ByteString (ByteString)
import qualified Data.Encoding as E
import qualified Data.Encoding.GB18030 as E
import qualified Data.Encoding.ShiftJIS as E
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Language.PyMO.GameConfig as P
import Main.Utf8 (withUtf8)
import System.Environment (getArgs)
import System.FilePath ((</>))

type Encoding = (String, T.Text -> ByteString)

data Arguments = Arguments
  { pymoDir :: FilePath,
    encoding :: Encoding,
    outputDir :: Maybe FilePath
  }

defaultGbkEncoding :: Encoding
defaultGbkEncoding = ("gbk", E.encodeStrictByteString E.GB18030 . T.unpack)

parseArgs :: [String] -> Maybe Arguments
parseArgs [] = Nothing
parseArgs (pymoDir' : []) = Just $ Arguments pymoDir' (defaultGbkEncoding) Nothing
parseArgs (pymoDir' : outputDir' : []) =
  Just $ Arguments pymoDir' defaultGbkEncoding (Just outputDir')
parseArgs (pymoDir' : outputDir' : "--encoding" : encoding' : []) = do
  encoding'' <- case encoding' of
    "gbk" -> Just defaultGbkEncoding
    "utf8" -> Just ("utf8", T.encodeUtf8)
    "sjis" -> Just ("sjis", E.encodeStrictByteString E.ShiftJIS . T.unpack)
    _ -> Nothing
  return $ (flip $ Arguments pymoDir') (Just outputDir') encoding''
parseArgs _ = Nothing

printHelp :: IO ()
printHelp = do
  putStrLn "PyMO2NScr"
  putStrLn "用于将 PyMO 游戏转换为 NScripter / ONScripter 游戏"
  putStrLn ""
  putStrLn "用法:"
  putStrLn "    PyMO2NScr <pymo-dir> [output-dir] [--encoding <gbk/en/sjis>]"
  putStrLn ""
  putStrLn "参数说明:"
  putStrLn "    <pymo-dir>                   PyMO的安装目录，必需参数"
  putStrLn "    [output-dir]                 输出目录"
  putStrLn "    [--encoding <gbk/en/sjis>]   输出编码"
  putStrLn "        gbk                      适用于汉化的 NScripter， MiNE，ONScripter-Jh 等"
  putStrLn "        en                       适用于 ONScripter-EN"
  putStrLn "        sjis                     适用于原版 ONScripter 和 NScripter"
  putStrLn ""

process :: Arguments -> IO ()
process args = do
  gameConfig <- P.loadGameConfig $ (pymoDir args </> "gameconfig.txt")
  putStrLn $ "PyMOGameConfig: " ++ T.unpack (P.getTextValue (T.pack "gametitle") gameConfig)
  putStrLn ""
  putStrLn $ "输出目录: " ++ show (outputDir args)
  putStrLn $ "目标编码: " ++ fst (encoding args)

main :: IO ()
main = withUtf8 $ do
  args <- getArgs
  case parseArgs args of
    Nothing -> printHelp
    Just args' -> process args'
