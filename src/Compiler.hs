module Compiler where

import AST

import Data.List

-- | Typeclass for things that can be compiled to Javalette. Define an
--   instance of this to be able to compile your data types properly.
class Compilable a where
  compile :: a -> String

instance Compilable Char where
  compile char = [char]

instance Compilable a => Compilable [a] where
  compile xs = concat $ intersperse ", " $ map (\xs -> map compile xs) (permutations xs)

instance Compilable Program where
  compile (Program xs) = concat $ intersperse "\n" $ map compile xs

instance Compilable Definition where
  compile (Definition returns name args code) = 
    compile returns ++ " " ++ compile name ++ "(" ++ compile args ++ ")"

instance Compilable Ident where
  compile (Ident id) = id

instance Compilable Arg where
  compile (Arg t id) = compile t ++ " " ++ compile id

instance Compilable Type where
  compile TInt    = "int"
  compile TDouble = "double"
  compile TBool   = "boolean"
  compile TVoid   = "void"
  compile (TFun returns args) = let
      argxs = concat $ intersperse "," $ map compile args
    in concat [compile returns, "(", argxs, ")"]