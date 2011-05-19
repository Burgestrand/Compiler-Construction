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

-- General helper methods:
infix 2 ?
True  ? x = const x
False ? x = id

-- Code that does NOT emit stuff and is independent from LLVM:

-- | True if the given expression is a literal
is_literal :: Expr -> Bool
is_literal (EInt _)    = True
is_literal (EDouble _) = True
is_literal (EBool _)   = True
is_literal _           = False

-- | Given a Type, return an LLVM type
llvm_type :: Type -> String
llvm_type TInt = "i32"
llvm_type TDouble = "double"
llvm_type TBool = "i1"
llvm_type TVoid = "void"

-- | Given an expression, return its’ LLVM type
llvm_expr_type :: Expr -> String
llvm_expr_type (ETyped t e) | is_literal e = llvm_type t
llvm_expr_type (EString _) = "i8*"

-- | Given a string, calculate its’ type
llvm_string_type str = "[" ++ show (length str + 1) ++ " x i8]"

-- | Retrieve the LLVM value of a literal expression
llvm_value_of :: Expr -> String
llvm_value_of e | is_literal e = value_of e
 where
   value_of (EInt x)    = show x
   value_of (EDouble x) = show x
   value_of (EBool LTrue)  = "true"
   value_of (EBool LFalse) = "false"

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
  lp <- gets labelPlaced
  when lp (emit x)

-- | Emit a label with a given name, jumping to it if nececery
putLabel name = do
  goto name
  emit (name ++ ":")
  modify (\state -> state { labelPlaced = True })
  
-- | Branch to a label
goto name = do
  emitCode ("br label %" ++ name)
  modify (\state -> state { labelPlaced = False })
  
-- | Generates a new number (for labels, vars or other fun stuff (where fun = consecutive)) 
getFun = do
  fun <- gets count
  modify (\state -> state { count = fun + 1 }) -- Even more fun!
  return fun

getLabel = do
  label <- getFun
  return "lab_" ++ (show label)

-- | Remembers the last fun value
bookmark :: LLVM Integer
bookmark = do
  num <- gets count
  return (num - 1)

-- | Retrieve the most recently generated variable
pull = (\n -> "%var_" ++ show n) `fmap` bookmark
   
-- | Generates a temp var and sets it to the arg
push t val = do
    var <- getVar
    emitCode (var ++ " = " ++ (llvm_type t) ++ " " ++ val)
  where
    getVar = do
    name <- getFun
    return "%var_" ++ (show name)

class (Show x) => Compileable x where
  assemble :: x -> LLVM ()

instance Compileable Definition where
  assemble (Definition returns (Ident name) args code) = do
      -- Add this functions’ name to the global state:
      modify (\state -> state { function = name })
      
      -- Readjust the output
      let llvm_returns = llvm_type returns
      let llvm_name = "@" ++ (name == "main" ? "" $ "f_") ++ name
      let llvm_args = args
      
      -- Finally emit function body
      pass $ do
        emit $ "define " ++ llvm_returns ++ " " ++ llvm_name ++ "()"
        emit "{"
        putLabel "entry"
        assemble code
        emit "}"
        
        globals <- gets globals
        names   <- mapM g_name [0..length globals]
        let declarations = intercalate "\n" $ map g_declare (zip globals names)
        return ((), (declarations:))
      
      emit ""
    where
      g_declare (str, name) = concat
        [name, " = internal constant", llvm_string_type str ," c\"", str ,"\\00\""]

instance Compileable Block where
  assemble (Block code) = mapM_ assemble code
  
instance Compileable Statement where
  assemble (SEmpty) = undefined
  assemble (SExpr e)  = assemble e
  
  assemble (SReturnV) = emit "ret void"
  assemble (SReturn (ETyped t e)) | is_literal e = do
    emit ("ret " ++ llvm_type t ++ " " ++ llvm_value_of e)
  
  assemble e = error ("implement assemble: " ++ show e)
  
instance Compileable Expr where
  -- Literals
  assemble (ETyped t e) | is_literal e = push t e
  assemble (EString str) = do
    g_var <- g_create str
    num   <- getFun
    var   <- pull
    emit (var ++ " = getelementptr " ++ llvm_string_type str ++ "* " ++ g_var ++ ", i32 0, i32 0")
  
  assemble (ETyped t (ECall (Ident func) args)) = do
      let prefix    = if builtin func then "" else "f_"
      let llvm_name = "@" ++ prefix ++ func
      
      let arg_types = map llvm_expr_type args
      arg_vars <- mapM (\x -> assemble x >> pull) args
      let llvm_args = intercalate "," [ t ++ " " ++ v | (t, v) <- zip arg_types arg_vars]
      
      emit $ "call " ++ llvm_type t ++ " " ++ llvm_name ++ "(" ++ llvm_args ++ ")"
    where
      builtin "printString" = True
      builtin "printInt"    = True
      builtin "printDouble" = True
      builtin "readInt"     = True
      builtin "readDouble"  = True
      builtin _ = False
  
  assemble e = error ("implement assemble: " ++ show e)

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
