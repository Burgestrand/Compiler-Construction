{-# LANGUAGE CPP #-}
module Compiler.Bogus (compile) where

import AST
import Directory
import System.FilePath (combine)
import System.IO.Unsafe

compile :: String -> Program -> String
compile name program = unlines
  ["declare void @printString(i8*)",
   "",
   "@g_1 = internal constant [13 x i8] c\"hello world\\0A\\00\"",
   "",
   "define i32 @main()",
   "{",
   "entry:",
   "  call void @foo()",
   "  ret i32 0",
   "}",
   "",
   "define void @foo()",
   "{",
   "entry:",
   "  %l_1 = getelementptr [ 13 x i8 ]* @g_1, i32 0, i32 0",
   "  call void @printString(i8* %l_1)",
   "  ret void",
   "}"]