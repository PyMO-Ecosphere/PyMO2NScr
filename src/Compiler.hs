module Compiler
  ( compile ) where

import CompilerMonad
import NScrGen
import Language.PyMO.Game
import Control.Monad.IO.Class (liftIO)
import Language.PyMO.Script
import Control.Monad (forM_)
import Commands
import qualified Data.HashMap.Strict as HM


compileScript :: ScriptName -> Script -> Compiler
compileScript scriptName script = do
  scriptLabel <- pymoLabelToNScrLabel scriptName Nothing
  newline
  commentLine $ "PyMO Script: " ++ scriptName
  defineLabel scriptLabel

  forM_ script $ \stmt ->
    case HM.lookup (stmtCommand stmt) commands of
      Nothing -> undefined
      Just command -> command $ stmtArgs stmt



compile :: FilePath -> Compiler
compile gameDir' = do
  game <- liftIO $ loadGame gameDir'

  (startSection, ()) <-
    intercept $ forM_ (gameScripts game) $ uncurry compileScript

  defineLabel "define"

  genLine "game"

  defineLabel "start"
  gen startSection

