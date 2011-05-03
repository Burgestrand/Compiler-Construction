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
  locals :: (Int, [Map Ident (Int, Type)]),
  
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
  assemble :: x -> Jasmin ()

-- > Low Level

-- | Emit a piece of code verbatim.
emit :: Code -> Jasmin ()
emit x = tell [x]

-- | Modify the stack with a user-given function, returning the return value.
stackmod :: (Stack -> Stack) -> Jasmin ()
stackmod fn = do
  stack <- (fn . stack) `fmap` get
  modify (\state -> state { stack = stack })

-- | Increase the stack size, returning the new Stack.
stackinc :: Jasmin ()
stackinc = stackmod (\(x, y) -> (x + 1, max (x + 1) y))

-- | Decrease the stack size, returnin gthe new Stack.
stackdec :: Jasmin ()
stackdec = stackmod (\(x, y) -> (x - 1, y))

-- | Generating a new unique label, returning that label
getlabel :: Jasmin Code
getlabel = do
  label <- ((1+) . label) `fmap` get
  modify (\state -> state { label = label })
  return ("lab" ++ show label)

-- > High Level

-- | Emit a directive with the specified name and parameters.
directive :: String -> String -> Jasmin ()
directive name args = emit ("." ++ name ++ " " ++ args)

-- | Push an expression constant, also modifying the stack.
push :: Expr -> Jasmin ()
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
call :: String -> [Expr] -> Type -> Jasmin ()
call func targs returns = do
    mapM (const stackdec) targs               -- decrease stack once for each arg
    unless (returns == TVoid)   (void stackinc) -- increase stack once for return type
    when   (returns == TDouble) (void stackinc) -- increase stack once again for doubles
  
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
    builtin "readDouble"  = True
    builtin _             = False

-- | Return a value of a given type.
jreturn :: Type -> Jasmin ()
jreturn TDouble = emit "dreturn"
jreturn TBool   = emit "ireturn"
jreturn TInt    = emit "ireturn"
jreturn TVoid   = emit "return"

-- | Place a label here
putlabel :: Code -> Jasmin ()
putlabel l = emit (l ++ ":")
 
-- | Goto another label
goto :: Code -> Jasmin ()
goto l = emit ("goto " ++ l)

-- | Go to another label if value on top of stack is 0
goto_if_zero :: Code -> Jasmin ()
goto_if_zero l = emit ("ifeq " ++ l)

-- | Negate the previous expression (double or integer)
neg :: Type -> Jasmin ()
neg TDouble = emit "dneg"
neg TInt    = emit "ineg"

-- | Emits a nop nop!
nop :: Jasmin ()
nop = emit "nop"

-- | Pop a thingy of type Type off the stack
pop :: Type -> Jasmin ()
pop TVoid   = emit ""
pop TDouble = do
  stackdec
  stackdec
  emit "pop2"
pop _       = do
  stackdec
  emit "pop"

(+>) :: Type -> Code -> Jasmin ()
TDouble +> code = emit ("d" ++ code)
_       +> code = emit ("i" ++ code)

-- | Fetch a local variable by putting it on the stack.
fetchVar :: Ident -> Jasmin Type
fetchVar name = do
    (i, tp) <- (find name . snd) `fmap` gets locals
    stackinc
    load tp (show i)
    return tp
  where
    find id (x:xs) | Map.member id x = (Map.!) x id
                   | otherwise       = find id xs
    find id _ = error $ (show id) ++ " not found"
    load TDouble i = stackinc >> emit ("dload " ++ i)
    load _       i = emit ("iload " ++ i)

-- | Declares a new variable in the current scope
declareVar :: Ident -> Type -> Jasmin ()
declareVar name tp = do
    (size, (scope:scopes)) <- gets locals
    
    let scope' = Map.insert name (size, tp) scope
    modify (\state -> state { locals = (size + sizeof tp, (scope':scopes)) })
  where
    sizeof TDouble  = 2
    sizeof _        = 1
    
-- | Store a literal in a local variable; returns its’ index.
storeVar :: Ident -> Jasmin ()
storeVar name = do
    (i, tp) <- (find name . snd) `fmap` gets locals
    
    stackdec
    store tp (show i)
  where
    find id (x:xs) | Map.member id x = (Map.!) x id
                   | otherwise       = find id xs
    store TDouble i = stackdec >> emit ("dstore " ++ i)
    store _       i = emit ("istore " ++ i)

inScope :: Jasmin a -> Jasmin a
inScope f = do
   (size, scopes) <- gets locals
   modify (\state -> state { locals = (size, (Map.empty:scopes)) })
   x <- f
   (size, scopes) <- gets locals
   modify (\state -> state { locals = (size, tail scopes) })
   return x

---

instance Compileable Definition where
  assemble (Definition returns (Ident name) args code) = do
      mapM declareArg args
      let args' = intercalate "," $ map writeArg args
      let signature  = "public static " ++ name
    
      directive "method" (signature ++ "(" ++ args' ++ ")" ++ type2str returns)
      pass $ do
        case code of
          (Block []) -> jreturn TVoid
          _          -> assemble code
        (_, stack) <- gets stack
        locals <- fst `fmap` gets locals
        return ((), (\code -> limits stack locals:indent code))
      directive "end" "method"
    where
      indent xs = map ("  " ++) xs
      limits stack locals = ".limit stack " ++ show stack ++ "\n.limit locals " ++ show locals
      declareArg (Arg t id) = declareVar id t
      writeArg (Arg t x) = type2str t

instance Compileable Block where
  assemble (Block code) = inScope $ mapM_ assemble code

instance Compileable Statement where
  assemble (SEmpty) = nop
  assemble (SIf (ETyped _ (EBool LFalse)) _) = nop
  assemble (SIf (ETyped _ (EBool LTrue))  s) = assemble s
  assemble (SIf e s) = do
    skiplabel <- getlabel
    assemble e
    goto_if_zero skiplabel
    assemble s
    putlabel skiplabel
    nop
    
  assemble (SIfElse (ETyped _ (EBool LTrue))  s1 _) = assemble s1
  assemble (SIfElse (ETyped _ (EBool LFalse)) _ s2) = assemble s2
  assemble (SIfElse e s1 s2) = do
    elselabel <- getlabel
    endlabel <- getlabel
    assemble e
    goto_if_zero elselabel
    assemble s1
    goto endlabel
    putlabel elselabel
    assemble s2
    putlabel endlabel
    nop
    
  assemble (SWhile e s) = do
    testlabel <- getlabel
    endlabel <- getlabel
    putlabel testlabel
    assemble e
    goto_if_zero endlabel
    assemble s
    goto testlabel
    putlabel endlabel
    nop
  assemble (SInc id) = do
    fetchVar id
    push (EInt 1)
    emit "iadd"
    stackdec
    storeVar id
  assemble (SDec id) = do
    fetchVar id
    push (EInt 1)
    emit "idec"
    stackdec
    storeVar id
       
  
  assemble (SBlock e) = assemble e
  assemble (SAss name e@(ETyped tp _)) = do
    assemble e
    storeVar name
    
  assemble (SReturnV) = jreturn TVoid
  assemble (SReturn e@(ETyped tp _)) = do
    assemble e
    jreturn tp
  
  assemble (SExpr e@(ETyped tp _)) = do
    (stacksize, _) <- gets stack
    assemble e
    pop tp
    
    (stacksize', _) <- gets stack
    when (stacksize /= stacksize') (fail $ "Incorrect stack size in expr " ++ show e ++ " before: " ++ show stacksize ++ ", after: " ++ show stacksize') 
    
  assemble (SDeclaration tp ds) = mapM_ declare ds
    where
      declare (DInit name e@(ETyped tp _)) = do 
          declareVar name tp 
          assemble e 
          storeVar name
      declare (DNoInit name) = do
          declareVar name tp
          push (initial tp) 
          storeVar name
        where
          initial TDouble = (EDouble 0)
          initial _       = (EInt 0)
  
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
    stackdec
    emit $ (case op of
      EQU -> "ifeq " 
      NE  -> "ifne ")++ lab_t
    stackdec
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
    stackdec
    emit $ (case op of
      LTH -> "iflt "
      LE  -> "ifle "
      GTH -> "ifgt "
      GE  -> "ifge ") ++ lab_t
    stackdec
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
  
  assemble (ETyped tp (EVar name)) = void (fetchVar name)
  assemble e = error $ "Non-compilable expression: " ++ show e

---

compiler :: (Compileable x) => String -> x -> Code
compiler name x = intercalate "\n" $ execWriter $ runStateT (assemble x) state
  where state = Compilation name (0, 0) (0, [Map.empty]) 0

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
