module Compiler (compile) where

import AST

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

---

type Jasmin = StateT Compilation (Writer Lines)

type Lines = [Code]
type Code  = String
type Stack = (Integer, Integer) -- | (Current, Maximum)
data Compilation = Compilation {
  -- | Class name
  name :: String,
  
  -- | (Current, Maximum)
  stack :: Stack,
  
  -- | Maps a variable name to its’ index and its’ type
  locals :: Map Ident (Int, Type),
  
  -- | Label counter
  label :: Integer
}

--
-- Utility
--

-- | True if the given expression returns a double.
is_double :: Expr -> Bool
is_double (EDouble _) = True
is_double _           = False

-- | True if the given expression is a literal.
is_literal :: Expr -> Bool
is_literal (EInt _)    = True
is_literal (EDouble _) = True
is_literal (EBool _)   = True
is_literal (EString _) = True
is_literal _           = False

-- | Gives the string type of a given expression.
type_of :: Expr -> String
type_of (EInt _)     = "I"
type_of (EDouble _)  = "D"
type_of (EBool _)    = "I"
type_of (EString _)  = "Ljava/lang/String;" -- ?!
type_of (ETyped t _) = type2str t

-- | Convert a type to a string (similar to type_of)
type2str :: Type -> String
type2str TInt    = "I"
type2str TDouble = "D"
type2str TBool   = "I"
type2str TVoid   = "V"

-- | Convert an expression to a Type
expr2type :: Expr -> Type
expr2type (EDouble _) = TDouble
expr2type _           = TInt

-- | Given an expression, always return unit.
void :: (Monad m) => m a -> m ()
void m = m >> return ()

---

class Compileable x where
  assemble :: x -> Jasmin Code

-- > Low Level

-- | Emit a piece of code verbatim.
emit :: Code -> Jasmin Code
emit x = tell [x] >> return x

-- | Modify the stack with a user-given function, returning the return value.
stackmod :: (Stack -> Stack) -> Jasmin Stack
stackmod fn = do
  stack <- (fn . stack) `fmap` get
  modify (\state -> state { stack = stack })
  return stack

-- | Increase the stack size, returning the new Stack.
stackinc :: Jasmin Stack
stackinc = stackmod (\(x, y) -> (x + 1, max (x + 1) y))

-- | Decrease the stack size, returnin gthe new Stack.
stackdec :: Jasmin Stack
stackdec = stackmod (\(x, y) -> (x - 1, y))

-- | Generating a new unique label, returning that label
getlabel :: Jasmin Code
getlabel = do
  label <- ((1+) . label) `fmap` get
  modify (\state -> state { label = label })
  return ("lab" ++ show label)

-- > High Level

-- | Emit a directive with the specified name and parameters.
directive :: String -> String -> Jasmin Code
directive name args = emit ("." ++ name ++ " " ++ args)

-- | Push an expression constant, also modifying the stack.
push :: Expr -> Jasmin Code
push expr = do
    let value = case expr of
                (EInt x)    -> show x
                (EDouble x) -> show x
                (EString x) -> show x
                (EBool LTrue)  -> "1"
                (EBool LFalse) -> "0"
    
    fn <- if is_double expr
          then stackinc >> return "ldc2_w "
          else return "ldc "
    
    stackinc
    emit $ fn ++ value

-- | Call a static function: name, args, returns
call :: String -> [Expr] -> Type -> Jasmin Code
call func targs returns = do
    mapM (const stackdec) targs               -- decrease stack once for each arg
    unless (returns == TVoid) (void stackinc) -- increase stack once for return type
  
    klass <- if builtin func then return "Runtime" else gets name
    let args = intercalate "," (map type_of targs)
    let name = klass ++ "/" ++ func
    emit $ "invokestatic " ++ name ++ "(" ++ args ++ ")" ++ type2str returns
  where
    -- Unfortunately, we need to hard-code the built in functions at the mo'
    builtin "printString" = True
    builtin "printInt"    = True
    builtin "printDouble" = True
    builtin "readInt"     = True
    builtin "readdouble"  = True
    builtin _             = False

-- | Return a value of a given type.
jreturn :: Type -> Jasmin Code
jreturn TDouble = emit "dreturn"
jreturn TBool   = emit "ireturn"
jreturn TInt    = emit "ireturn"
jreturn TVoid   = emit "return"

-- | Place a label here
putlabel :: Code -> Jasmin Code
putlabel l = emit (l ++ ":")
 
-- | Goto another label
goto :: Code -> Jasmin Code
goto l = emit ("goto " ++ l)

-- | Negate the previous expression (double or integer)
neg :: Type -> Jasmin Code
neg TDouble = emit "dneg"
neg TInt    = emit "ineg"

-- | Fetch a local variable by putting it on the stack.
fetchVar :: Ident -> Jasmin (Int, Type)
fetchVar name = do
    (i, tp) <- find name `fmap` gets locals
    stackinc
    emit $ load tp ++ " " ++ (show i)
    return (i, tp)
  where
    find = flip (Map.!)
    load TDouble = "dload"
    load _       = "iload"

-- | Store a literal as a local variable; returns its’ index.
storeVar :: Ident -> Expr -> Jasmin Int
storeVar name e | is_literal e = do
    let tp = expr2type e
    localVars <- gets locals
    
    -- if a new variable then store its index!
    when (isJust $ Map.lookup name localVars) $ do
      let locals' = Map.insert name (Map.size localVars, tp) localVars
      modify (\state -> state { locals = locals' })
    
    -- find the variables’ index
    (i, _) <- find name `fmap` gets locals
    
    stackdec
    emit $ store tp ++ " " ++ (show i)
    return i
  where
    find = flip (Map.!)
    store TDouble = "dstore"
    store _       = "istore"

---

instance Compileable Definition where
  assemble (Definition returns (Ident name) args code) = do
    args <- intercalate "," `fmap` mapM assemble args
    let signature  = "public static " ++ name
    
    directive "method" (signature ++ "(" ++ args ++ ")" ++ type2str returns)
    pass $ do
      case code of
        (Block []) -> jreturn TVoid
        _          -> assemble code
      (_, s) <- gets stack
      locals <- Map.size `fmap` gets locals
      return ((), ([".limit stack " ++ show s, ".limit locals " ++ show locals] ++ ))
    directive "end" "method"

instance Compileable Arg where
  assemble (Arg t x) = return $ type2str t

instance Compileable Block where
  assemble (Block code) = concat `fmap` mapM assemble code

instance Compileable Statement where
  assemble (SReturnV) = jreturn TVoid
  assemble (SReturn e@(ETyped tp _)) = do
    assemble e
    jreturn tp
  
  assemble (SExpr e) = assemble e
  
  assemble e = error $ "Non-compilable statement: " ++ show e

instance Compileable Expr where
  assemble e | is_literal e = push e
  assemble (ETyped t e) | is_literal e = assemble e
  assemble (ETyped returns (ECall (Ident func) args)) = do
    mapM_ assemble args
    call func args returns
  assemble (ETyped tp (ENeg e)) = do
    assemble e
    neg tp
  assemble (ETyped TBool (ENot e)) = do
    lab_t <- getlabel
    lab_f <- getlabel
    assemble e
    emit $ "ifne " ++ lab_t
    push (EBool LFalse)
    goto lab_f
    putlabel lab_t
    push (EBool LTrue)
    putlabel lab_f    
  assemble (ETyped tp (EMul e1 op e2)) = do
    assemble e1
    assemble e2
    stackdec
    emit $ (case tp of
      TInt    -> "i"
      TDouble -> "d") ++ (case op of
      Times -> "mul"
      Div   -> "div"
      Mod   -> "rem")
  assemble (ETyped tp (EAdd e1 op e2)) = do
    assemble e1
    assemble e2
    stackdec
    emit $ (case tp of
      TInt    -> "i"
      TDouble -> "d") ++ (case op of
      Plus  -> "add"
      Minus -> "sub")
  assemble (ETyped TBool (EEqu e1 op e2)) = do
    lab_t <- getlabel
    lab_f <- getlabel
    assemble e1
    assemble e2
    let (ETyped tp _) = e1
    emit $ (case tp of
      TBool   -> "i"
      TInt    -> "i"
      TDouble -> "d") ++ "sub"
    emit $ (case op of
      EQU -> "ifeq " 
      NE  -> "ifne ")++ lab_t
    push (EBool LFalse)
    goto lab_f
    putlabel lab_t
    push (EBool LTrue)
    putlabel lab_f
  assemble (ETyped TBool (ERel e1 op e2)) = do
    lab_t <- getlabel
    lab_f <- getlabel
    assemble e1
    assemble e2
    let (ETyped tp _) = e1
    emit $ (case tp of
      TInt    -> "i"
      TDouble -> "d") ++ "sub"
    emit $ (case op of
      LTH -> "iflt "
      LE  -> "ifle "
      GTH -> "ifgt "
      GE  -> "ifge ") ++ lab_t
    push (EBool LFalse)
    goto lab_f
    putlabel lab_t
    push (EBool LTrue)
    putlabel lab_f
  assemble (ETyped TBool (EAnd e1 e2)) = do
    assemble e1
    assemble e2
    stackdec
    emit "iand"
  assemble (ETyped TBool (EOr e1 e2)) = do
    assemble e1
    assemble e2
    stackdec
    emit "ior"
  
  
  assemble e = error $ "Non-compilable expression: " ++ show e

---

compiler :: (Compileable x) => String -> x -> Code
compiler name x = intercalate "\n" $ execWriter $ runStateT (assemble x) state
  where state = Compilation name (0, 0) Map.empty 0

---

compile :: String -> Program -> Code
compile name (Program fs) = header ++ functions
  where header = unlines [".class public " ++ name,
                          ".super java/lang/Object\n",
                          ".method public static main([Ljava/lang/String;)V",
                          "  invokestatic " ++ name ++ "/main()I",
                          "  invokestatic java/lang/System/exit(I)V",
                          "  return",
                          ".end method"] ++ "\n"
        functions = intercalate "\n\n" (map (compiler name) fs)
