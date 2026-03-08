{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Compiler (module Compiler) where

import qualified Control.Monad.Error.Class as E
import qualified Control.Monad.Except as E
import qualified Control.Monad.RWS as RWS
import qualified TextBuilder as TB
import qualified Language.PyMO.GameConfig as PyMO
import qualified Language.PyMO.Script as PyMO
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text.Encoding as T
import qualified Language.PyMO.AssetDatabase as AD
import GHC.Generics (Generic)
import Data.FileEmbed (embedFile)
import System.FilePath ((</>))
import Data.Hashable (Hashable)
import System.Exit (exitFailure)
import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.IO.Class (liftIO)

-- Compiler Input
data CompilerInput = CompilerInput
  { ciPyMOGameConfig :: PyMO.GameConfig
  , ciPyMOGameDir :: FilePath
  , ciAssetDatabase :: AD.AssetDatabase }

makeCompilerInput :: FilePath -> ResourceT IO CompilerInput
makeCompilerInput gameDir = do
  gameConfig <- liftIO $ PyMO.loadGameConfig $ gameDir </> "gameconfig.txt"
  ad <- AD.openAssetDatabase gameDir gameConfig
  return $ CompilerInput gameConfig gameDir ad

getCompilerInput :: (CompilerInput -> a) -> Compiler a
getCompilerInput = Compiler . RWS.asks

-- Compiler State
type PyMOVarName = T.Text
type NScrVarName = TB.TextBuilder
type ScriptName = T.Text
type ScriptId = Int

data CompilerState = CompilerState
  { csLocalVariables :: HM.HashMap PyMOVarName T.Text
  , csGlobalVariables :: HM.HashMap PyMOVarName T.Text
  , csRuntimeGlobalVariables :: HS.HashSet T.Text
  , csLoadedScripts :: HM.HashMap ScriptName (ScriptId, PyMO.Script)
  , csCompiledScripts :: HS.HashSet ScriptName
  , csLabelCount :: Int }

emptyCompilerState :: CompilerState
emptyCompilerState = CompilerState
  { csLocalVariables = mempty
  , csGlobalVariables = mempty
  , csRuntimeGlobalVariables = mempty
  , csLoadedScripts = mempty
  , csCompiledScripts = mempty
  , csLabelCount = 10 }

updateCompilerState :: (CompilerState -> CompilerState) -> Compiler ()
updateCompilerState = Compiler . RWS.modify

getCompilerState :: (CompilerState -> a) -> Compiler a
getCompilerState = Compiler . RWS.gets

defineRuntimeGlobalVariables :: T.Text -> Compiler ()
defineRuntimeGlobalVariables nscrVarName =
  updateCompilerState $ \x ->
    x { csRuntimeGlobalVariables = HS.insert nscrVarName $ csRuntimeGlobalVariables x}

pymoVarToNSVar :: PyMOVarName -> Compiler NScrVarName
pymoVarToNSVar pymoVarName = do
  let isGlobalVar = "S" `T.isPrefixOf` pymoVarName
  varSet <- getCompilerState $
    if isGlobalVar
      then csGlobalVariables
      else csLocalVariables

  case HM.lookup pymoVarName varSet of
    Just x -> return $ TB.text x
    Nothing -> do
      let nscrVarNamePrefix = if isGlobalVar then "PYMOG_" else "PYMOL_"
      let nscrVarName = nscrVarNamePrefix <> T.pack (show (HM.size varSet))
      updateCompilerState $ \x ->
        if isGlobalVar
          then x { csGlobalVariables = HM.insert pymoVarName nscrVarName varSet }
          else x { csLocalVariables = HM.insert pymoVarName nscrVarName varSet }
      return $ TB.text nscrVarName

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

-- Asset

newtype AssetKey = AssetKey (AD.AssetKind, AD.AssetNameLowered)
  deriving ( Eq, Show, Generic )

instance Hashable AssetKey

type AssetName = T.Text

addAsset :: PyMO.Stmt -> AD.AssetKind -> AssetName -> ImageAssetHasMask -> Compiler ()
addAsset stmt assetKind' assetName' imageAssetHasMask = do
  ad <- getCompilerInput ciAssetDatabase
  asset <- liftIO $ AD.getAssetRef ad assetKind' $ T.unpack assetName'

  case asset of
    Nothing ->
      throwWithStmt stmt $ "未能找到资源 " ++ show assetKind' ++ ": " ++ show assetName' ++ " 。"
    Just asset' ->
      let k = AssetKey (assetKind', AD.arNameLowered asset') in
      writeCompilerOutput mempty { coAssets = HM.singleton k (asset', imageAssetHasMask) }

-- Compiler Output

type Hole = TB.TextBuilder

type ImageAssetHasMask = Bool

data CompilerOutput = CompilerOutput
  { coHeader :: Hole
  , coDefines :: Hole
  , coBoot :: Hole
  , coBody :: Hole
  , coAssets :: HM.HashMap AssetKey (AD.AssetReference, ImageAssetHasMask) }

instance Semigroup CompilerOutput where
  a <> b = CompilerOutput
    { coHeader = coHeader a <> coHeader b
    , coDefines = coDefines a <> coDefines b
    , coBoot = coBoot a <> coBoot b
    , coBody = coBody a <> coBody b
    , coAssets = coAssets a <> coAssets b }

instance Monoid CompilerOutput where
  mempty = CompilerOutput mempty mempty mempty mempty mempty
  mappend = (<>)

writeCompilerOutput :: CompilerOutput -> Compiler ()
writeCompilerOutput output = Compiler (RWS.tell output)

newline :: TB.TextBuilder
newline = TB.string "\n"

writeHeader :: Hole -> Compiler ()
writeHeader hole = writeCompilerOutput mempty { coHeader = hole }

writeDefine :: Hole -> Compiler ()
writeDefine hole = writeCompilerOutput mempty { coDefines = hole <> newline }

writeBoot :: Hole -> Compiler ()
writeBoot hole = writeCompilerOutput mempty { coBoot = hole <> newline }

writeBody :: Hole -> Compiler ()
writeBody hole = writeCompilerOutput mempty { coBody = hole <> newline }

-- Compiler

type CompilerError = (Maybe PyMO.Stmt, String)

newtype Compiler x =
  Compiler (RWS.RWST CompilerInput CompilerOutput CompilerState (E.ExceptT CompilerError IO) x)

instance Functor Compiler where
  fmap f (Compiler rwst) = Compiler (fmap f rwst)

instance Applicative Compiler where
  pure x = Compiler (pure x)
  Compiler f <*> Compiler x = Compiler (f <*> x)

instance Monad Compiler where
  return = pure
  Compiler m >>= f = Compiler (m >>= (\x -> let Compiler r = f x in r))

instance RWS.MonadIO Compiler where
  liftIO = Compiler . liftIO

instance E.MonadError CompilerError Compiler where
  throwError e = Compiler (RWS.lift (E.throwError e))
  catchError (Compiler m) handler = Compiler $ E.catchError m (\e -> let Compiler r = handler e in r)

logInfo :: String -> Compiler ()
logInfo = liftIO . putStrLn

stmtMsg :: PyMO.Stmt -> String
stmtMsg stmt =
  "在脚本 "++ PyMO.stmtScriptName stmt
  ++ " 第" ++ show (PyMO.stmtLineNumber stmt)
  ++ "行："

warnWithStmt :: PyMO.Stmt -> String -> Compiler ()
warnWithStmt stmt msg = warn $ stmtMsg stmt ++ msg

warn :: String -> Compiler ()
warn msg = logInfo $ "【警告】" ++ msg

throwWithStmt :: PyMO.Stmt -> String -> Compiler a
throwWithStmt stmt msg = E.throwError ((Just stmt), msg)

runCompiler :: CompilerInput -> Compiler () -> IO (Either CompilerError T.Text)
runCompiler ci (Compiler compiler) = do
  result <- E.runExceptT $ RWS.runRWST compiler ci emptyCompilerState
  case result of
    Left x -> return $ Left x
    Right ((), _, co) -> Right . T.unlines <$> (mapM (applyHole co) nscrTemplate)
  where
    nscrTemplate = T.lines $ T.decodeUtf8 $(embedFile "./src/nscr-template.txt")
    holeMapping =
      [ ("header", coHeader)
      , ("defines", coDefines)
      , ("boot", coBoot)
      , ("body", coBody) ]
    applyHole co line =
      case T.stripPrefix "&&" line of
        Nothing -> return line
        Just holeName ->
          case lookup holeName holeMapping of
            Nothing -> fail $ "UNKNOWN HOLE: "++ T.unpack holeName
            Just holeGetter -> return $ TB.toText $ holeGetter co


runCompilerExitIfFailed :: CompilerInput -> Compiler () -> IO T.Text
runCompilerExitIfFailed c compiler = do
  result <- runCompiler c compiler
  case result of
    Right x -> return x
    Left (stmt, msg) -> do
      putStrLn ""
      putStrLn "== 编译失败 =="
      putStrLn ""
      case stmt of
        Nothing -> return ()
        Just stmt' -> do
          putStrLn $ stmtMsg stmt'
          putStrLn ""
      putStrLn $ "\t" ++ msg
      putStrLn ""
      putStrLn ""
      exitFailure
