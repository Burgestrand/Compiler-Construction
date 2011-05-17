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

type LLVM = StateT Compilation (Writer Lines)

type Lines = [Code]
type Code  = String
data Compilation = Compilation { 
  -- | Lists all locals
  locals :: [Ident],
  
  -- | Label and temp counter
  count :: Integer,
  
  -- | Global, duh
  globals :: Map String Ident,
  
  -- | Can generate code?
  labelPlaced :: Bool
}

--

-- | Emit a line of LLVM assembly
emit :: (MonadWriter [t] m) => t -> m ()
emit x = tell [x]

-- | Emit a label with a given name
label name = emit (name ++ ":")

--

class Compileable x where
  assemble :: x -> LLVM ()

instance Compileable Definition where
  assemble (Definition returns (Ident name) args code) = do
    let llvm_returns = type_of returns
    let llvm_name = name
    let llvm_args = args
    
    emit "define " ++ llvm_returns ++ " @" ++ llvm_name ++ "()"
    emit "{"
    label "entry"
    
    emit "}"

instance Compileable Block where
  assemble (Block code) = undefined
  
instance Compileable Statement where
  assemble (SEmpty) = undefined

--

compile :: String -> Program -> Code
compile (Program fs) = header ++ functions
  where header = unlines [] ++ "\n"
        functions = intercalate "\n\n" (map compiler fs)
        
compiler :: (Compileable x) => String -> x -> Code
compiler x = intercalate "\n" $ execWriter $ runStateT (assemble x) state
  where state = Compilation [] 0 Map.empty False