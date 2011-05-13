module Compiler (compile) where

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
  -- | Lists all locals
  locals :: [Ident],
  
  -- | Label and temp counter
  count :: Integer,
  
  -- | Global, duh
  globals :: Map String Ident
}

class Compileable x where
  assemble :: x -> LLVM ()

--


