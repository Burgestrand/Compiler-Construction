module Main where

import ErrM
import Parser
import TypeChecker
import Compiler

import System.Environment (getArgs)
import System.Exit (exitFailure)

-- aliases
parselex = pProgram . myLexer

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      source  <- readFile file
      program <- return $ do
        program <- parselex source
        program <- typecheck program
        return (compile program)
      
      case program of
        Ok  source  -> putStr source
        Bad message -> error message
      
    _      -> error "Usage: jlc path/to/javalette/source.jl"