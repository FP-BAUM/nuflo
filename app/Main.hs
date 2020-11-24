module Main where

import System.IO(hPutStrLn, stderr, hSetBuffering,
                 stdin, stdout, BufferMode(..))
import System.Environment(getArgs)

import Error(Error(..))
import Position(positionRegion)
import Lexer.Lexer(tokenize)
import Parser.Reader(readSource)
import Parser.Parser(parse, parseAndGetNamespace)
import Infer.KindInference(inferKinds)
import Infer.TypeInference(inferTypeWithMain)
import Desugaring.Desugaring(desugarProgram)
import Eval.Eval(evalInNamespace)

import TestMain(runAllTests)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  args <- getArgs
  run args

run :: [String] -> IO ()
run ["-T"]        = runAllTests
run ["-t", input] = runTokenizer input
run ["-r", input] = runReader input
run ["-p", input] = runParser input
run ["-k", input] = runKindInference input
run ["-i", input] = runTypeInference input
run ["-d", input] = runDesugaring input
run [input]       = runEvaluator input
run _             = usage

runTokenizer :: String -> IO ()
runTokenizer filename = do
  source <- readFile filename
  case tokenize filename source of
    Left  e      -> die e
    Right tokens -> mapM_ (putStrLn . show) tokens

runReader :: String -> IO ()
runReader filename = do
  res <- readSource filename
  case res of
    Left  e      -> die e
    Right tokens -> mapM_ (putStrLn . show) tokens

runParser :: String -> IO ()
runParser filename = do
  res <- readSource filename
  case res of
    Left  e      -> die e
    Right tokens -> do
      case parse tokens of
        Left e        -> die e
        Right program -> putStrLn (show program)

runKindInference :: String -> IO ()
runKindInference filename = do
  res <- readSource filename
  case res of
    Left  e      -> die e
    Right tokens -> do
      case parse tokens of
        Left e        -> die e
        Right program -> do
          case inferKinds program of
            Left e    -> die e
            Right ()  -> putStrLn "OK"

runTypeInference :: String -> IO ()
runTypeInference filename = do
  res <- readSource filename
  case res of
    Left  e      -> die e
    Right tokens -> do
      case parse tokens of
        Left e        -> die e
        Right program -> do
          case inferKinds program of
            Left e    -> die e
            Right () -> do
              case inferTypeWithMain program of
                Left e -> die e
                Right program' -> putStrLn (show program')

runDesugaring :: String -> IO ()
runDesugaring filename = do
  res <- readSource filename
  case res of
    Left  e      -> die e
    Right tokens -> do
      case parse tokens of
        Left e        -> die e
        Right program -> do
          case inferKinds program of
            Left e    -> die e
            Right () -> do
              case inferTypeWithMain program of
                Left e -> die e
                Right program' -> do
                  case desugarProgram program' of
                    Left e -> die e
                    Right termC -> putStrLn (show termC)

runEvaluator :: String -> IO ()
runEvaluator filename = do
  res <- readSource filename
  case res of
    Left  e      -> die e
    Right tokens -> do
      case parseAndGetNamespace tokens of
        Left e        -> die e
        Right (program, namespace) -> do
          case inferKinds program of
            Left e    -> die e
            Right () -> do
              case inferTypeWithMain program of
                Left e -> die e
                Right program' -> do
                  case desugarProgram program' of
                    Left e -> die e
                    Right termC -> evalInNamespace namespace termC

usage :: IO ()
usage = do
  putStrLn "Usage:"
  putStrLn "  la -T              Run tests."
  putStrLn "  la -t foo.nu       Tokenize file."
  putStrLn "  la -r foo.nu       Tokenize file (including dependencies)."
  putStrLn "  la -p foo.nu       Parse file."
  putStrLn "  la -k foo.nu       Infer kinds."
  putStrLn "  la -i foo.nu       Infer types."
  putStrLn "  la -d foo.nu       Desugar program."
  putStrLn "  la foo.nu          Eval program."

die :: Error -> IO ()
die e = do
  hPutStrLn stderr ("---ERROR---")
  hPutStrLn stderr (positionRegion (errorPosition e))
  hPutStrLn stderr "---"
  hPutStrLn stderr (errorMessage e)

