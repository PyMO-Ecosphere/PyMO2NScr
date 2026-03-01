module CommandCompiler (commands) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Compiler

type Command = T.Text
type CommandHandler = Compiler ()

commands :: HM.HashMap Command CommandHandler
commands = HM.fromList []
