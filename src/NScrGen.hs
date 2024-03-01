{-# LANGUAGE OverloadedStrings #-}

module NScrGen
  ( gen
  , gen'
  , newline
  , defineLabel
  , genLine
  , genLine'
  , commentLine
  , defineLabel'
  , cmd
  , NScrArg (..)
  ) where

import CompilerMonad
import ToBuilder
import Control.Monad.RWS (tell, modify)
import Data.Text.Lazy.Builder (Builder)
import Data.Text (Text, replace)
import Control.Monad (unless)


gen :: Builder -> Compiler
gen = gen'


gen' :: ToBuilder b => b -> Compiler
gen' = tell . toBuilder


genLine :: Builder -> Compiler
genLine = genLine'


genLine' :: ToBuilder b => b -> Compiler
genLine' x = gen' x >> newline


commentLine :: ToBuilder b => b -> Compiler
commentLine x = gen "; " >> genLine' x


newline :: Compiler
newline = gen' '\n'


defineLabel :: Builder -> Compiler
defineLabel = defineLabel'


defineLabel' :: ToBuilder b => b -> Compiler
defineLabel' label = do
  modify $ \x -> x { nscrLabels = nscrLabels x + 1 }
  gen "*"
  gen' label
  newline


data NScrArg
  = Str Text
  | Num Int
  | Label Builder


instance ToBuilder NScrArg where
  toBuilder (Str x) = mconcat
    [ toBuilder '\"'
    , toBuilder $ replace "\"" "" x
    , toBuilder '\"']
  toBuilder (Num x) = toBuilder x
  toBuilder (Label x) = mappend (toBuilder '*') x


cmd :: Text -> [NScrArg] -> Compiler
cmd cmd' args' = do
  gen' cmd'
  unless (null args') $ gen' ' '
  let args :: [NScrArg] -> Builder
      args [] = mempty
      args [x] = toBuilder x
      args (x:y:xs) =
        mappend
          (mconcat
            [ toBuilder x
            , toBuilder ','
            , toBuilder y ])
          (args xs)
  gen $ args args'
  newline

