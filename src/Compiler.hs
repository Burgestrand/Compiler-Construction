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
  -- | (Current, Maximum)
  stack :: Stack
}

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
stackyay :: Jasmin Stack
stackyay = stackmod (\(x, y) -> (x + 1, max (x + 1) y))

-- | Decrease the stack size, returnin gthe new Stack.
stackboo :: Jasmin Stack
stackboo = stackmod (\(x, y) -> (x - 1, y))

-- > High Level

-- | Emit a directive with the specified name and parameters.
directive :: String -> String -> Jasmin Code
directive name args = emit ("." ++ name ++ " " ++ args)

-- | Push an expression constant, also modifying the stack.
push :: Expr -> Jasmin Code
push expr = do
    let value = case expr of
                (EInt x)    -> show x
                (EString x) -> show x
                (EBool LTrue)  -> "1"
                (EBool LFalse) -> "0"
    
    fn <- if is_double expr
          then stackyay >> return "ldc2_w "
          else return "ldc "
    
    stackyay
    emit $ fn ++ value
  where
    is_double (EDouble _) = True
    is_double _           = False
    

-- | Return a value of a given type.
-- TODO: Modify stack?
jreturn :: Type -> Jasmin Code
jreturn TDouble = emit "dreturn"
jreturn TBool   = emit "ireturn"
jreturn TInt    = emit "ireturn"
jreturn TVoid   = emit "return"

---

instance Compileable Definition where
  assemble (Definition returns (Ident name) args (Block code)) = do
    args <- intercalate ";" `fmap` mapM assemble args
    let signature  = "public static " ++ name
    let returntype = case returns of
                     TInt    -> "I"
                     TDouble -> "D"
                     TBool   -> "B"
                     TVoid   -> "V"
    
    directive "method" (signature ++ "(" ++ args ++ ")" ++ returntype)
    
    -- always return 0 in all methods ftw
    directive "limit" "stack 1"
    emit "iconst_0"
    emit "ireturn"
    
    directive "end" "method"

instance Compileable Arg where
  assemble (Arg t x) = return $ case t of
    TInt    -> "I"
    TDouble -> "D"
    TBool   -> "B"

---

compiler :: (Compileable x) => x -> Code
compiler x = intercalate "\n" $ execWriter $ runStateT (assemble x) state
  where state = Compilation (0, 0) 

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
        functions = intercalate "\n\n" (map compiler fs)