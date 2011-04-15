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
data Compilation = Compilation {
  stacksize   :: Integer, -- | Your average counter
  maxstack    :: Integer  -- | Highest value the counter ever reaches
}

---

class Compileable x where
  assemble :: x -> Jasmin Code

emit :: Code -> Jasmin Code
emit x = tell [x] >> return x

-- | Emit a directive with the specified name and parameters.
directive :: String -> String -> Jasmin Code
directive name args = emit ("." ++ name ++ " " ++ args)

-- | Emit a method declaration and then assemble the contents.
method :: (Compileable x) => String -> [Arg] -> x -> Jasmin Code
method name args body = undefined

---

instance Compileable Definition where
  assemble (Definition return (Ident name) args (Block code)) = do
    args <- intercalate ";" `fmap` mapM assemble args
    directive "method" ("public static " ++ name ++ "(" ++ args ++ ")")
    directive "end" "method"

instance Compileable Arg where
  assemble (Arg t x) = return $ case t of
    TInt    -> "I"
    TDouble -> "D"
    TBool   -> "B"

---

compiler :: (Compileable x) => x -> Code
compiler x = intercalate "\n" $ execWriter $ runStateT (assemble x) (Compilation 0 0) 

---

compile :: String -> Program -> Code
compile name (Program fs) = header ++ functions
  where header = unlines [".class public " ++ name,
                          ".super java/lang/Object",
                          ".method public static main([Ljava/lang/String;)V",
                          "  invokestatic " ++ name ++ "/main()I",
                          "  invokestatic java/lang/System/exit(I)V",
                          ".end method"] ++ "\n"
        functions = intercalate "\n\n" (map compiler fs)