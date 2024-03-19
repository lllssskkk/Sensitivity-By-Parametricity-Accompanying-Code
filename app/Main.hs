{-# OPTIONS_GHC -Wno-name-shadowing #-}
-- Turn off unused import warning off in stub
{-# OPTIONS_GHC -Wno-unused-imports #-}

import Control.Monad (forM_, when)
import Control.Monad.Except (runExcept)
import Control.Monad.State (StateT, execStateT, gets, liftIO, modify)

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, readFile, stderr)

import Fun.Abs (Program)
import Fun.Par (myLexer, pProgram)
import Fun.Print (printTree)

import NbELambda (interpret)

-- | Entry point.
main :: IO ()
main = do
  filepath <- getArgs
  if null filepath
    then usage
    else readFile (head filepath) >>= run

-- | Main pipeline.
run :: String -> IO ()
run s = eval =<< parse s

-- | Parse.
parse :: String -> IO Program
parse s = case pProgram (myLexer s) of
  Left err -> do
    putStrLn "SYNTAX ERROR"
    putStrLn err
    exitFailure
  Right prg -> return prg

-- | Interpret in call-by-value or call-by-name.
eval :: Program -> IO ()
eval prg = do
  case runExcept $ interpret prg of
    Left err -> do
      putStrLn "INTERPRETER ERROR"
      putStrLn err
      exitFailure
    Right i -> do
      putStrLn $ show i
      hPutStrLn stderr "OK"
      exitSuccess

-- * Command-line parsing

-- | Print usage information and exit.
usage :: IO ()
usage = do
  putStrLn "Given the path to the Source File"
  exitFailure
