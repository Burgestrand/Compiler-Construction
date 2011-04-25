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
is_literal _           = False

-- | Gives the type of a given expression.
type_of :: Expr -> Type
type_of (EInt _)    = TInt
type_of (EDouble _) = TDouble
type_of (EBool _)   = TBool

-- | Gives the type as a string of a given type.
typestring :: Type -> String
typestring TInt    = "I"
typestring TDouble = "D"
typestring TBool   = "I"
typestring TVoid   = "V"

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
                (EDouble x) -> show x
                (EString x) -> show x
                (EBool LTrue)  -> "1"
                (EBool LFalse) -> "0"
    
    fn <- if is_double expr
          then stackyay >> return "ldc2_w "
          else return "ldc "
    
    stackyay
    emit $ fn ++ value
    

-- | Return a value of a given type.
-- TODO: Modify stack?
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
    let returntype = typestring returns
    
    directive "method" (signature ++ "(" ++ args ++ ")" ++ returntype)
    pass $ do
      assemble code
      (_, s) <- gets stack
      return ((), ((".limit stack " ++ show s):))
    directive "end" "method"

instance Compileable Arg where
  assemble (Arg t x) = return $ typestring t

instance Compileable Block where
  assemble (Block code) = concat `fmap` mapM assemble code

instance Compileable Statement where
  assemble SReturnV = jreturn TVoid
  assemble (SReturn e@(ETyped tp _)) = do
    assemble e
    jreturn tp
  
  assemble e = error $ "Uncompilable statement: " ++ show e

instance Compileable Expr where
  assemble (ETyped t e) | is_literal e = push e
  assemble x = error $ show x ++ " isn't wrapped in ETyped! I want wrapper!"

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
