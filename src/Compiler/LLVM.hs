module Compiler.LLVM (compile) where

import AST

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

--

data BasicBlock = Returning   Lines 
                | Jumping     Lines Label 
                | Conditional Lines Label Label

type LLVM = StateT Compilation (Writer (Map Label BasicBlock))

type Lines = [Code]
type Code  = String
data Compilation = Compilation {
  -- | Class name
  name :: String,
  
  -- | Lists all locals
  locals :: [Ident],
  
  -- | Label counter
  label :: Integer
  
  -- | Temp variable counter
  temp :: Integer
}
