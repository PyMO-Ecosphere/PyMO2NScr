{-# LANGUAGE OverloadedStrings #-}

module Commands
  ( pymoLabelToNScrLabel
  , commands) where

import CompilerMonad
import Data.Text (Text, unpack)
import Data.Maybe (fromJust)
import Language.PyMO.Script
import Data.Text.Lazy.Builder (Builder)
import IDAllocator (getID)
import ToBuilder
import Control.Monad.RWS (asks)
import NScrGen
import Data.HashMap.Strict (HashMap, fromList)


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
commands = fromList
  [ ("label", label) ]

