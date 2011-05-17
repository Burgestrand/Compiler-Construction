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

-- Code that does NOT emit stuff and is independent from LLVM:

-- | Given a Type, return an LLVM type
type_of :: Type -> String
type_of TInt = "i32"
type_of TDouble = "double"
type_of TBool = "i1"
type_of TVoid = "void"

-- Code that DOES emit stuff:

-- | Emit a line of LLVM assembly
emit x = tell [x]

-- | Emit a bit of code iff it's possible
emitCode x = do
  lp <- gets labelPlaced
  when lp (emit x)

-- | Emit a label with a given name, jumping to it if nececery
putlabel name = do
  goto name
  emit (name ++ ":")
  modify (\state -> state { labelPlaced = True })
  
-- | Branch to a label
goto name = do
  emitCode ("br " ++ name)
  modify (\state -> state { labelPlaced = False })

class Compileable x where
  assemble :: x -> LLVM ()

instance Compileable Definition where
  assemble (Definition returns (Ident name) args code) = do
    let llvm_returns = type_of returns
    let llvm_name = name
    let llvm_args = args
    
    emit $ "define " ++ llvm_returns ++ " @" ++ llvm_name ++ "()"
    emit "{"
    label "entry"
    assemble code
    emit "}"

instance Compileable Block where
  assemble (Block code) = emit "*code*"
  
instance Compileable Statement where
  assemble (SEmpty) = undefined
  
instance Compileable Expr where
  assemble (EInt) = undefined

--

compile :: String -> Program -> Code
compile _ (Program fs) = header ++ "\n\n" ++ functions
  where functions = intercalate "\n\n" (map compiler fs)
        header = unlines 
          ["declare void @printString(i8*)",
           "declare void @printInt(i32)",
           "declare void @printDouble(double)",
           "declare i32 @readInt()",
           "declare double @readDouble()"]
        
compiler :: (Compileable x) => x -> Code
compiler x = intercalate "\n" $ execWriter $ runStateT (assemble x) state
  where state = Compilation [] 0 Map.empty False
