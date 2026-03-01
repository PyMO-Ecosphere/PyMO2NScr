{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Compiler
  ( CompilerInput
  , Compiler
  , ScriptName
  , ScriptId
  , makeCompilerInput
  , loadPyMOScript
  , logInfo
  , runCompiler ) where

import Control.Monad.RWS (RWST, runRWST, modify, gets, asks)
import qualified Control.Monad.RWS as RWS
import qualified TextBuilder (TextBuilder, toText)
import qualified Language.PyMO.GameConfig as PyMO
import qualified Language.PyMO.Script as PyMO
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text.Encoding as T
import Data.FileEmbed (embedFile)
import System.FilePath ((</>))

-- Compiler Input
data CompilerInput = CompilerInput
  { ciPyMOGameConfig :: PyMO.GameConfig
  , ciPyMOGameDir :: FilePath }

makeCompilerInput :: FilePath -> IO CompilerInput
makeCompilerInput gameDir = do
  gameConfig <- PyMO.loadGameConfig $ gameDir </> "gameconfig.txt"
  return $ CompilerInput gameConfig gameDir

getCompilerInput :: (CompilerInput -> a) -> Compiler a
getCompilerInput = Compiler . asks

-- Compiler State
type PyMOVarName = T.Text
type NScrVarName = T.Text
type ScriptName = T.Text
type ScriptId = Int

data CompilerState = CompilerState
  { csLocalVariables :: HM.HashMap PyMOVarName NScrVarName
  , csGlobalVariables :: HM.HashMap PyMOVarName NScrVarName
  , csLoadedScripts :: HM.HashMap ScriptName (ScriptId, PyMO.Script)
  , csCompiledScripts :: HS.HashSet ScriptName }

emptyCompilerState :: CompilerState
emptyCompilerState = CompilerState
  { csLocalVariables = mempty
  , csGlobalVariables = mempty
  , csLoadedScripts = mempty }

updateCompilerState :: (CompilerState -> CompilerState) -> Compiler ()
updateCompilerState = Compiler . modify

getCompilerState :: (CompilerState -> a) -> Compiler a
getCompilerState = Compiler . gets

pymoVarToNsVar :: PyMOVarName -> Compiler NScrVarName
pymoVarToNsVar pymoVarName = do
  let isGlobalVar = "S" `T.isPrefixOf` pymoVarName
  varSet <- getCompilerState $
    if isGlobalVar
      then csGlobalVariables
      else csLocalVariables

  case HM.lookup pymoVarName varSet of
    Just x -> return x
    Nothing -> do
      let nscrVarNamePrefix = if isGlobalVar then "PYMO_G_" else "PYMO_S_"
      let nscrVarName = T.pack $ nscrVarNamePrefix <> show (HM.size varSet)
      updateCompilerState $ \x ->
        if isGlobalVar
          then x { csGlobalVariables = HM.insert pymoVarName nscrVarName varSet }
          else x { csLocalVariables = HM.insert pymoVarName nscrVarName varSet }
      return nscrVarName

loadPyMOScript :: ScriptName -> Compiler (ScriptId, PyMO.Script)
loadPyMOScript scriptName = do
  let scriptNameLowered = T.toLower scriptName
  loadedScripts <- getCompilerState csLoadedScripts
  case HM.lookup scriptNameLowered loadedScripts of
    Just x -> return x
    Nothing -> do
      gameDir <- getCompilerInput ciPyMOGameDir
      script <- liftIO $ PyMO.loadPyMOScript gameDir (T.unpack scriptName)
      let scriptId = HM.size loadedScripts
          loadedScripts' = HM.insert scriptNameLowered (scriptId, script) loadedScripts
      updateCompilerState $ \x -> x { csLoadedScripts =  loadedScripts' }
      return (scriptId, script)

isScriptCompiled :: ScriptName -> Compiler Bool
isScriptCompiled scriptName =
  HS.member (T.toLower scriptName) <$> getCompilerState csCompiledScripts

markAsCompiled :: ScriptName -> Compiler ()
markAsCompiled scriptName =
  updateCompilerState $ \x -> x {
    csCompiledScripts = HS.insert (T.toLower scriptName) (csCompiledScripts x) }

-- Compiler Output
type Hole = TextBuilder.TextBuilder

data CompilerOutput = CompilerOutput
  { coHeader :: Hole
  , coDefines :: Hole
  , coBody :: Hole }

instance Semigroup CompilerOutput where
  a <> b = CompilerOutput
    { coHeader = coHeader a <> coHeader b
    , coDefines = coDefines a <> coDefines b
    , coBody = coBody a <> coBody b
    }

instance Monoid CompilerOutput where
  mempty = CompilerOutput mempty mempty mempty
  mappend = (<>)

-- Compiler
newtype Compiler x =
  Compiler (RWST CompilerInput CompilerOutput CompilerState IO x)

instance Functor Compiler where
  fmap f (Compiler rwst) = Compiler (fmap f rwst)

instance Applicative Compiler where
  pure x = Compiler (pure x)
  Compiler f <*> Compiler x = Compiler (f <*> x)

instance Monad Compiler where
  return = pure
  Compiler m >>= f = Compiler (m >>= (\x -> let Compiler r = f x in r))

liftIO :: IO a -> Compiler a
liftIO = Compiler . RWS.liftIO

logInfo :: String -> Compiler ()
logInfo = liftIO . putStrLn

runCompiler :: CompilerInput -> Compiler () -> IO T.Text
runCompiler ci (Compiler compiler) = do
  ((), _, co) <- runRWST compiler ci emptyCompilerState
  return $ T.unlines $ fmap (applyHole co) nscrTemplate
  where
    nscrTemplate = T.lines $ T.decodeUtf8 $(embedFile "./src/nscr-template.txt")
    holeMapping =
      [ (T.pack "header", coHeader)
      , (T.pack "defines", coDefines)
      , (T.pack "body", coBody) ]
    applyHole co line =
      case T.stripPrefix "&&" line of
        Nothing -> line
        Just holeName ->
          case lookup holeName holeMapping of
            Nothing -> T.concat [ "<UNKNOWN HOLE: ", holeName, ">" ]
            Just holeGetter -> TextBuilder.toText $ holeGetter co
