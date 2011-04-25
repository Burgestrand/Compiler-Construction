module Compiler (compile) where

import AST

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer

import Data.List

---

type Jasmin = StateT Compilation (Writer Lines)

type Lines = [Code]
type Code  = String
type Stack = (Integer, Integer) -- | (Current, Maximum)
data Compilation = Compilation {
  -- | Class name
  name :: String,
  
  -- | (Current, Maximum)
  stack :: Stack
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
type_of (EString _)  = "Ljava/lang/String" -- ?!
type_of (ETyped t _) = type2str t

-- | Convert a type to a string (similar to type_of)
type2str :: Type -> String
type2str TInt    = "I"
type2str TDouble = "D"
type2str TBool   = "I"
type2str TVoid   = "V"

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
  
  klass <- gets name
  let args = intercalate ";" (map type_of targs)
  let name = klass ++ "/" ++ func
  emit $ "invokestatic " ++ name ++ "(" ++ args ++ ")" ++ type2str returns

-- | Return a value of a given type.
jreturn :: Type -> Jasmin Code
jreturn TDouble = emit "dreturn"
jreturn TBool   = emit "ireturn"
jreturn TInt    = emit "ireturn"
jreturn TVoid   = emit "return"

---

instance Compileable Definition where
  assemble (Definition returns (Ident name) args code) = do
    args <- intercalate ";" `fmap` mapM assemble args
    let signature  = "public static " ++ name
    
    directive "method" (signature ++ "(" ++ args ++ ")" ++ type2str returns)
    pass $ do
      assemble code
      (_, s) <- gets stack
      return ((), ((".limit stack " ++ show s):))
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
  
  assemble e = error $ "Non-compilable expression: " ++ show e

---

compiler :: (Compileable x) => String -> x -> Code
compiler name x = intercalate "\n" $ execWriter $ runStateT (assemble x) state
  where state = Compilation name (0, 0) 

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
