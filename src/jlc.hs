module Main where

import ErrM
import Parser

import System.Environment (getArgs)
import System.Exit (exitFailure)

-- | Compile the file at the given path containing jasmine code.
jasmin :: FilePath -> IO ()
jasmin = undefined

-- aliases
parselex = pProgram . myLexer

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      program <- parselex `fmap` readFile file
      case program of
        Ok  x -> print x
        Bad s -> error s
    _      -> error "Usage: jlc path/to/javalette/source.jl"