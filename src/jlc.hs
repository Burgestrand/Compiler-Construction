module Main where

import ErrM
import Parser
import TypeChecker
import Compiler

import Control.Monad
import Data.Char (toUpper)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (dropExtensions, takeBaseName)
import System.IO

-- utility
titleize :: String -> String
titleize (x:xs) = (toUpper x):xs

-- | For some reason, the supplied Grade requires output on stderr!?
info :: String -> IO ()
info msg = hPutStr stderr msg

-- aliases
parselex = pProgram . myLexer

-- | Compile a file given its’ name.
jasmin source = undefined

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
          (Bad message) -> do
            info $ "ERROR: " ++ message
            exitFailure
          
          (Ok source) -> do
            info "OK"
            writeFile (name ++ ".j") source
      
      _      -> error "Usage: jlc path/to/javalette/source.jl"
  where
    isOk (Ok _) = True
    isOk _      = False