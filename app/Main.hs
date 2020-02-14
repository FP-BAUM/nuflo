module Main where

import System.Environment

import Lexer.Lexer(tokenize)
import Parser.Reader(readSource)

import TestMain(runAllTests)

main :: IO ()
main = do
  args <- getArgs
  run args

run :: [String] -> IO ()
run ["-T"]        = runTests
run ["-t", input] = runTokenizer input
run ["-r", input] = runReader input
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

runReader :: String -> IO ()
runReader filename = do
  res <- readSource filename
  case res of
    Left  e      -> putStrLn $ show e
    Right tokens -> mapM_ (putStrLn . show) tokens

usage :: IO ()
usage = do
  putStrLn "Usage:"
  putStrLn "  la -T              Run tests."
  putStrLn "  la -t foo.la       Tokenize file."
  putStrLn "  la -r foo.la       Tokenize file, including dependencies."

