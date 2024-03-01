{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Text.Builder
import Data.Text (unpack)
import Data.Encoding (Encoding, encodeLazyByteString)
import Data.Encoding.GB18030 (GB18030 (GB18030))
import qualified Data.ByteString.Lazy as BS
import Data.Bits (xor)
import System.FilePath ((</>))


saveResult :: Encoding enc => enc -> Builder -> Bool -> FilePath -> IO ()
saveResult enc result encrypted outDir =
  let bs = encodeLazyByteString enc $ unpack $ run result in
  if encrypted
    then BS.writeFile (outDir </> "nscript.dat") $ BS.map (xor 0x84) bs
    else BS.writeFile (outDir </> "0.txt") bs


main :: IO ()
main = do
  saveResult GB18030 "测试" False "./"
  putStrLn "hello world"
