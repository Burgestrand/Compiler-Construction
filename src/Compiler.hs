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

compile :: String -> Program -> String
compile name program = (++) header $ runIdentity $ do
    let (Program xs) = program
    return (show xs)
  where header = unlines [".class public " ++ name,
                          ".super java/lang/Object",
                          ".method public static main([Ljava/lang/String;)V",
                          "  invokestatic " ++ name ++ "/main()I",
                          "  invokestatic java/lang/System/exit(I)V",
                          ".end method"] ++ "\n"