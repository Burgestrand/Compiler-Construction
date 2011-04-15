module Compiler (compile) where

import AST

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer

type Compiling = WriterT Code (StateT Compilation Identity)

type Code = [String]
data Compilation = Compilation {
  stacksize   :: Integer, -- | Your average counter
  maxstack    :: Integer  -- | Highest value the counter ever reaches
}

compile :: Program -> String
compile program = unlines [
  ".class public Code",
  ".super java/lang/Object",
  ".method public static main([Ljava/lang/String;)V",
  "  .limit locals 1",
  "  .limit stack 2",
  "  ldc2_w 3.14",
  "  invokestatic Runtime/printDouble(D)V",
  "  return",
  ".end method"
  ]