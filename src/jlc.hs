module Main where

import ErrM
import Parser
import TypeChecker
import Compiler

import Data.Char (toUpper)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (dropExtensions, takeBaseName)

-- utility
titleize :: String -> String
titleize (x:xs) = (toUpper x):xs

-- aliases
parselex = pProgram . myLexer

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      let name = titleize $ dropExtensions (takeBaseName file)
      source  <- readFile file
      program <- return $ do
        program <- parselex source
        program <- typecheck program
        return $ compile name program
      
      case program of
        Ok  source  -> putStr source
        Bad message -> error message
      
    _      -> error "Usage: jlc path/to/javalette/source.jl"