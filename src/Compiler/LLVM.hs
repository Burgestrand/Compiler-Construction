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

  -- | List of scopes containing the variables in that scope
  -- NOTE: Could use Set for faster lookups
  locals :: [[Ident]],
  
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

-- Guard but it takes the monadic action afterwards
guardM bool m = guard (bool) >> m

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
llvm_expr_type (ETyped t e) = llvm_type t
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

-- | Given an expression return its’ type
expr_type :: Expr -> Type
expr_type (ETyped t _) = t

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
 
gotoif test name1 name2 = do
  emitCode ("br i1 " ++ test ++ ", label %" ++ name1 ++ ", label %" ++ name2)
  modify (\state -> state { labelPlaced = False })
  
  
-- | Generates a new number (for labels, vars or other fun stuff (where fun = consecutive)) 
getFun :: LLVM Integer
getFun = do
  fun <- gets count
  modify (\state -> state { count = fun + 1 }) -- Even more fun!
  return fun

getLabel = do
  label <- getFun
  return $ "lab_" ++ show label

-- | Remembers the last fun value
bookmark :: LLVM Integer
bookmark = do
  num <- gets count
  return (num - 1)
  
genVar = do
  getFun
  pull

-- | Retrieve the most recently generated variable
pull = (\n -> "%var_" ++ show n) `fmap` bookmark

-- | Assemble *and* pull
asspull e = assemble e >> pull
   
-- | Generates a temp var and sets it to the arg
pushWithPrefix p t val = do
    var <- getVar
    emitCode (var ++ " = " ++ p ++ " " ++ (llvm_type t) ++ " " ++ val)
  where
    getVar = do
    name <- getFun
    return $ "%var_" ++ show name
    
push = pushWithPrefix ""
    
-- | Emits a test expr; chooses a expr to use and joins it...
choose te e1 e2 = do
  test <- asspull te
  lab_true <- getLabel
  lab_false <- getLabel
  lab_end <- getLabel
  emitCode("br " ++ llvm_expr_type te ++ " " ++ test ++
           ", label %" ++ lab_true ++ ", label %" ++ lab_false)
  
  putLabel lab_true
  true_val <- asspull e1
  goto lab_end
  
  putLabel lab_false
  false_val <- asspull e2
  goto lab_end  
  
  putLabel lab_end
  let cases = "[" ++ true_val ++ ", %" ++ lab_true ++ "], ["++ false_val ++ ", %" ++ lab_false ++ "]"
  pushWithPrefix "phi" (expr_type e1) cases 
  
-- | Stores the given literal expression in the target variable
store :: Type -> String -> String -> LLVM ()
store t target source = do
  let instruction = "store " ++ llvm_type t ++ " " ++ source
  emitCode $ instruction ++ ", " ++ llvm_type t ++ "* " ++ target

-- | Allocates memory for a of given type
alloca :: String -> Type -> LLVM ()
alloca name t = emit $ name ++ " = alloca " ++ llvm_type t

-- | Retrieve the variable name of a true variable™
getIdent :: Ident -> LLVM String
getIdent ident = do
    scopes <- gets locals
    let (scopeNo, localNo) = find (reverse scopes)
    return ("%var." ++ show scopeNo ++ "." ++ show localNo ++ ".ptr")
  where
    find (scope:scopes) 
      | ident `elem` scope = (length scopes, fromJust $ ident `elemIndex` scope) 
      | otherwise = find scopes 

-- | Put a new local temporär permanent variable into the current scope
putIdent :: Ident -> LLVM String
putIdent ident = do
  scopes <- gets locals
  let locals = last scopes ++ [ident]
  modify (\state -> state { locals = init scopes ++ [locals] })
  getIdent ident

-- | Compare two operands with a given operation
compareExpr :: Expr -> Expr -> String -> LLVM ()
compareExpr e1@(ETyped tp _) e2 op = do
  e1var <- asspull e1
  e2var <- asspull e2
  
  let fn  = if tp == TDouble then "fcmp" else "icmp"
  let pre = if tp == TDouble then "o" else ""
  let cmp = pre ++ op
  
  pushWithPrefix (fn ++ " " ++ cmp) tp (e1var ++ ", " ++ e2var)

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
        emitCode (if returns == TVoid then "ret void" else "unreachable")
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
  assemble (SExpr e)  = assemble e
  
  assemble (SAss ident e@(ETyped t _)) = do
    from <- asspull e
    var <- getIdent ident
    store t var from
  
  assemble (SDeclaration t vars) = mapM_ declare vars
    where
      declare (DInit   ident e) = do
          val <- asspull e
          ptr <- putIdent ident
          alloca ptr t
          store t ptr val
      declare (DNoInit ident) = do
          ptr <- putIdent ident
          alloca ptr t
          store t ptr $ llvm_value_of (initial t)
        where
          initial (TDouble) = EDouble 0
          initial (TInt)    = EInt 0
          initial (TBool)   = EBool LFalse
  
  assemble (SReturnV) = do
    emitCode "ret void" 
    modify (\state -> state { labelPlaced = False })
  assemble (SReturn (ETyped t e)) | is_literal e = do
    emitCode ("ret " ++ llvm_type t ++ " " ++ llvm_value_of e)
    modify (\state -> state { labelPlaced = False })
  
  assemble (SEmpty) = return ()
    
  assemble (SBlock b) = do
    modify (\state -> state { locals = (locals state) ++ [[]] })
    assemble b
    modify (\state -> state { locals = init (locals state)})
    
  assemble (SInc id) = desugarincdec id Plus
                                              
  assemble (SDec id) = desugarincdec id Minus
  
  assemble (SIfElse e stm1 stm2) = do
    lab_true <- getLabel
    lab_false <- getLabel
    lab_end <- getLabel
    assemble e
    test <- pull
    gotoif test lab_true lab_false
    
    putLabel lab_true
    assemble stm1
    goto lab_end
  
    putLabel lab_false
    assemble stm2
    goto lab_end  
    
    putLabel lab_end
  
  assemble (SIf e stm) = assemble (SIfElse e stm SEmpty)
  
  assemble (SWhile e stm) = do
    lab_test <- getLabel
    lab_body <- getLabel
    lab_end <- getLabel
    putLabel lab_test
    assemble e
    test <- pull
    gotoif test lab_body lab_end
    
    putLabel lab_body
    assemble stm
    goto lab_test
    
    putLabel lab_end 
  
  assemble e = error ("implement assemble: " ++ show e)
  
desugarincdec id op  = assemble (
                         SAss id $
                           ETyped TInt $ EAdd (ETyped TInt (EVar id)) 
                                              op
                                              (ETyped TInt (EInt 1)))

instance Compileable Expr where
  assemble (ETyped t (EVar ident)) = do
    ident <- getIdent ident
    pushWithPrefix "load" t ("* " ++ ident)
  
  assemble (ETyped t e) | is_literal e = pushWithPrefix func t (llvm_value_of e ++ ", " ++ zero)
    where
      func = guardM (t == TDouble) "f" ++ "add"
      zero = "0" ++ guardM (t == TDouble) ".0"
  
  assemble (EString str) = do
    g_var <- g_create str
    num   <- getFun
    var   <- pull
    emitCode (var ++ " = getelementptr " ++ llvm_string_type str ++ "* " ++ g_var ++ ", i32 0, i32 0")
  
  -- Function calls
  assemble (ETyped t (ECall (Ident func) args)) = do
      let prefix    = if builtin func then "" else "f_"
      let llvm_name = "@" ++ prefix ++ func
      
      let arg_types = map llvm_expr_type args
      arg_vars <- mapM asspull args
      let llvm_args = intercalate "," [ t ++ " " ++ v | (t, v) <- zip arg_types arg_vars]
      
      let llvm_sig = " " ++ llvm_name ++ "(" ++ llvm_args ++ ")"
      
      if (t /= TVoid)
        then pushWithPrefix "call" t llvm_sig
        else emitCode ("call " ++ llvm_type t ++ llvm_sig)
          
    where
      builtin "printString" = True
      builtin "printInt"    = True
      builtin "printDouble" = True
      builtin "readInt"     = True
      builtin "readDouble"  = True
      builtin _ = False
  
  -- Logic operations
  assemble (ETyped t (ENot e)) = asspull e >>= \x -> pushWithPrefix "sub" t ("1, " ++ x)
  assemble (ETyped TBool (EEqu e1 op e2)) = compareExpr e1 e2 (if op == EQU then "eq" else "ne")
  assemble (ETyped TBool (ERel e1@(ETyped tp _) op e2)) = compareExpr e1 e2 (prefix ++ opOf op)
    where
      opOf LTH = "lt"
      opOf LE  = "le"
      opOf GTH = "gt"
      opOf GE  = "ge"
      prefix   = guardM (tp /= TDouble) "s"
  
  assemble (ETyped TBool (EAnd e1 e2)) = choose e1 e2 (ETyped TBool (EBool LFalse))
  assemble (ETyped TBool (EOr e1 e2))  = choose e1 (ETyped TBool (EBool LTrue)) e2
  
  -- Arithmetic operations
  assemble (ETyped t (ENeg e)) = do
    val <- asspull e
    pushWithPrefix "sub" t ("0, " ++ val)
    
  assemble (ETyped t (EMul e1 op e2)) = do
    let oper = ((t == TDouble) `guardM` "f") ++ 
                 case op of
                    Times -> "mul"
                    Div   -> "sdiv"
                    Mod   -> "srem"
    val1 <- asspull e1
    val2 <- asspull e2
    pushWithPrefix oper t (val1 ++ ", " ++ val2)
    
  assemble (ETyped t (EAdd e1 op e2)) = do
    let oper = ((t == TDouble) `guardM` "f") ++ 
                  case op of
                    Plus  -> "add"
                    Minus -> "sub"
    val1 <- asspull e1
    val2 <- asspull e2
    pushWithPrefix oper t (val1 ++ ", " ++ val2)
  
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
  where state = Compilation undefined [[]] 0 [] False
