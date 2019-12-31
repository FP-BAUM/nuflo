module Main where

import System.Environment

import Lexer

import qualified TestMain

main :: IO ()
main = do
  args <- getArgs
  run args

run :: [String] -> IO ()
run ["-t"] = runTests
run _      = usage

runTests :: IO ()
runTests = do
  putStrLn "----------------------------------------"
  TestMain.runAllTests 

usage :: IO ()
usage = do
  putStrLn "Usage:"
  putStrLn "  ./lambda-unif -t              Run tests."
  putStrLn "  ./lambda-unif --tok foo.lu    Tokenize file."

