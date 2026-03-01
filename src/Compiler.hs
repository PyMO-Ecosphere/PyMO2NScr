{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Compiler
  ( CompilerInput
  , makeCompilerInput
  , runCompiler ) where

import Control.Monad.RWS (RWST, runRWST)
import qualified TextBuilder (TextBuilder, toText)
import qualified Language.PyMO.GameConfig as PyMO
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as T
import Data.FileEmbed (embedFile)
import System.FilePath ((</>))

data CompilerInput = CompilerInput
  { ciPyMOGameConfig :: PyMO.GameConfig
  , ciPyMOGameDir :: FilePath }

makeCompilerInput :: FilePath -> IO CompilerInput
makeCompilerInput gameDir = do
  gameConfig <- PyMO.loadGameConfig $ gameDir </> "gameconfig.txt"
  return $ CompilerInput gameConfig gameDir

data CompilerState = CompilerState
  { csVariables :: HM.HashMap T.Text Int }

emptyCompilerState :: CompilerState
emptyCompilerState = CompilerState
  { csVariables = HM.empty }

type Hole = TextBuilder.TextBuilder

data CompilerOutput = CompilerOutput
  { coHeader :: Hole
  , coDefines :: Hole
  , coBody :: Hole }

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

instance Semigroup CompilerOutput where
  a <> b = CompilerOutput
    { coHeader = coHeader a <> coHeader b
    , coDefines = coDefines a <> coDefines b
    , coBody = coBody a <> coBody b
    }

instance Monoid CompilerOutput where
  mempty = CompilerOutput mempty mempty mempty
  mappend = (<>)

runCompiler :: CompilerInput -> Compiler () -> IO T.Text
runCompiler ci (Compiler compiler) = do
  ((), _, co) <- runRWST compiler ci emptyCompilerState
  return $ T.unlines $ fmap (applyHole co) nscrTemplate
  where
    nscrTemplate = T.lines $ T.decodeUtf8 $(embedFile "./src/nscr-template.txt")
    holeMapping =
      [ ("header", coHeader)
      , ("defines", coDefines)
      , ("body", coBody) ]
    applyHole co line =
      case T.stripPrefix "&&" line of
        Nothing -> line
        Just holeName ->
          case lookup holeName holeMapping of
            Nothing -> T.concat [ "<UNKNOWN HOLE: ", holeName, ">" ]
            Just holeGetter -> TextBuilder.toText $ holeGetter co
