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
  -- | Name of the function (gives us function-local globals)
  function :: String,

  -- | Lists all locals
  locals :: [Ident],
  
  -- | Label and temp counter
  count :: Integer,
  
  -- | Global, duh
  globals :: [String],
  
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

-- | Retrieve the name of a global by a given number

-- Code that DOES NOT emit stuff, but is dependent on LLVM:

-- | Given an integer, return the name of the global variable
g_name int = do
  func <- gets function
  return $ intercalate "_" ["@g", func, show int]

-- | Create a global and return its’ (complete) name
g_create string = do
  count <- length `fmap` gets globals
  new_globals <- (++ [string]) `fmap` gets globals
  modify (\state -> state { globals = new_globals })
  g_name count

-- Code that DOES emit stuff:

-- | Emit a line of LLVM assembly
emit x = tell [x]

-- | Emit a bit of code iff it's possible
emitCode x = do
  lp <- labelPlaced `fmap` get
  when lp (emit x)

-- | Emit a label with a given name
label name = do
  -- goto name
  emit (name ++ ":")
  modify (\state -> state { labelPlaced = True })

class Compileable x where
  assemble :: x -> LLVM ()

instance Compileable Definition where
  assemble (Definition returns (Ident name) args code) = do
      -- Add this functions’ name to the global state:
      modify (\state -> state { function = name })
      
      let llvm_returns = type_of returns
      let llvm_name = "f_" ++ name
      let llvm_args = args
    
      pass $ do
        emit $ "define " ++ llvm_returns ++ " @" ++ llvm_name ++ "()"
        emit "{"
        label "entry"
        name <- g_create "coolio"
        emit "}"
        
        globals <- gets globals
        names   <- mapM g_name [0..length globals]
        let declarations = intercalate "\n" $ map g_declare (zip globals names)
        return ((), (declarations:))
      
      emit ""
    where
      g_declare (str, name) = concat
        [name, " = internal constant[", show (length str + 1), " x i8] c\"", str ,"\\00\""]

instance Compileable Block where
  assemble (Block code) = emit "*code*"
  
instance Compileable Statement where
  assemble (SEmpty) = undefined

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
  where state = Compilation undefined [] 0 [] False
