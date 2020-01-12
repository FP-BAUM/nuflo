module Main where

import System.Environment

import Lexer(tokenizeProgram)

import TestMain(runAllTests)

main :: IO ()
main = do
  args <- getArgs
  run args

run :: [String] -> IO ()
run ["-t"]        = runTests
run ["-T", input] = runTokenizer input
run _             = usage

runTests :: IO ()
runTests = do
  putStrLn "----------------------------------------"
  runAllTests 

runTokenizer :: String -> IO ()
runTokenizer filename = do
  source <- readFile filename
  putStrLn $ show (tokenizeProgram filename source)

usage :: IO ()
usage = do
  putStrLn "Usage:"
  putStrLn "  ./lambda-unif -t              Run tests."
  putStrLn "  ./lambda-unif -T foo.lu       Tokenize file."

