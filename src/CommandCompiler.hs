module CommandCompiler (commands) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Language.PyMO.Script as PyMO
import Compiler

type Command = T.Text
type CommandHandler = PyMO.Stmt -> Compiler ()

commands :: HM.HashMap Command CommandHandler
commands = HM.fromList []
