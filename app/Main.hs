module Main where

import System.Environment

import Lexer.Lexer(tokenize)

import TestMain(runAllTests)

main :: IO ()
main = do
  args <- getArgs
  run args

run :: [String] -> IO ()
run ["-T"]        = runTests
run ["-t", input] = runTokenizer input
run _             = usage


runTests :: IO ()
runTests = do
  putStrLn "----------------------------------------"
  runAllTests 

runTokenizer :: String -> IO ()
runTokenizer filename = do
  source <- readFile filename
  case tokenize filename source of
    Left  e      -> putStrLn $ show e
    Right tokens -> mapM_ (putStrLn . show) tokens

usage :: IO ()
usage = do
  putStrLn "Usage:"
  putStrLn "  ./lambda-unif -T              Run tests."
  putStrLn "  ./lambda-unif -t foo.la       Tokenize file."

