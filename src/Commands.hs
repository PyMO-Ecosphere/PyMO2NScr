{-# LANGUAGE OverloadedStrings #-}

module Commands
  ( pymoLabelToNScrLabel
  , commands) where

import CompilerMonad
import Data.Text (Text, pack ,unpack)
import Data.Maybe (fromJust)
import Language.PyMO.Script
import Data.Text.Lazy.Builder (Builder)
import IDAllocator (getID)
import ToBuilder
import Control.Monad.Trans.RWS (asks)
import NScrGen
import Data.HashMap.Strict (HashMap, fromList)
import Data.Bifunctor (first)


pymoLabelToNScrLabel :: ScriptName -> Maybe Text -> CompilerM Builder
pymoLabelToNScrLabel scriptName pymoLabelName = do
  pymoLabels' <- asks pymoLabels
  nscrLabelID <- getID (scriptName, pymoLabelName) pymoLabels'
  return $ mappend "PYMO_" $ toBuilder nscrLabelID


type Command = [Text] -> Compiler


label :: Command
label [labelName] = do
  scriptName <- asks $ stmtScriptName . fromJust . currentStmt
  nscrLabel <- pymoLabelToNScrLabel scriptName $ Just labelName
  newline
  commentLine $ "PyMO Label: " ++ scriptName ++ " - " ++ unpack labelName
  defineLabel nscrLabel
label _ = invalidArg


commands :: HashMap Text Command
commands = fromList $ first pack <$>
  [ ("label", label) ]

