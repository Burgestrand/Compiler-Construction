module Main where

import ErrM
import Parser
import TypeChecker
import Compiler

import Control.Monad
import Data.Char (toUpper)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath
import System.IO
import System.Process (rawSystem)
import System.Exit
import System.Directory

-- utility
titleize :: String -> String
titleize (x:xs) = (toUpper x):xs

-- | For some reason, the supplied Grade requires output on stderr!?
info :: String -> IO ()
info msg = hPutStr stderr (msg ++ "\n")

-- | Fail with an error message to STDERR.
fatal msg = info msg >> exitFailure

-- aliases
parselex = pProgram . myLexer

-- | Compile a file given its’ name.
jasmin :: String -> IO ExitCode
jasmin source = rawSystem "java" ["-jar", "lib/jasmin.jar", "-d", takeDirectory source, source]

main :: IO ()
main = do
    args <- getArgs
    case args of
      [file] -> do
        let name   = dropExtensions (takeBaseName file)
        target  <- canonicalizePath (dropExtensions file ++ ".j")
        source  <- readFile file
        program <- return $ do
          program <- parselex source
          program <- typecheck program
          return $ compile name program
        
        case program of
          (Bad message) -> do
            fatal $ "ERROR: " ++ message
          
          (Ok source) -> do
            info "OK"
            writeFile target source   -- write assembled code
            compiled <- jasmin target -- compile assembled code
            case compiled of
              (ExitFailure n) -> fatal $ "Compilation failed with exit code " ++ (show n)
              _               -> return ()
      
      _      -> error "Usage: jlc path/to/javalette/source.jl"
  where
    isOk (Ok _) = True
    isOk _      = False