module Compiler.Bogus (compile) where

import System.IO.Unsafe

compile :: String
compile = unsafePerformIO bogus

bogus :: IO String
bogus = do
  source <- lines `fmap` readFile "./Bogus.hs"
  let code = dropWhile (/= "{-") source
  let code' = takeWhile (/= "-}") (tail code)
  return (unlines code')

{-
define i32 main()
{
entry:
  ret i32 1
}
-}