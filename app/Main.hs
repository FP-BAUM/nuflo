module Main where

import System.IO(hPutStrLn, stderr, hSetBuffering,
                 stdin, stdout, BufferMode(..))
import System.Environment(getArgs)

import Control.Monad.Except

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

runComputationThen c k = 
  do r <- runExceptT c
     either die k r

runTokenizer :: String -> IO ()
runTokenizer filename = 
  runComputationThen
   (do source <- ExceptT (readFile filename >>= (return . Right))
       tokens <- ExceptT $ return $ tokenize filename source
       return tokens)
   (mapM_ (putStrLn . show))

runReader :: String -> IO ()
runReader filename = 
  runComputationThen
   (do tokens <- ExceptT $ readSource filename
       return tokens)
   (\tokens -> mapM_ (putStrLn . show) tokens)

runParser :: String -> IO ()
runParser filename = 
  runComputationThen
   (do tokens  <- ExceptT $ readSource filename
       program <- ExceptT $ return $ parse tokens
       return program)
   (\program -> putStrLn $ show program)

runKindInference :: String -> IO ()
runKindInference filename = 
  runComputationThen
   (do tokens  <- ExceptT $ readSource filename
       program <- ExceptT $ return $ parse tokens 
       ExceptT $ return $ inferKinds program)
   (\_ -> putStrLn "OK")

runTypeInference :: String -> IO ()
runTypeInference filename = 
  runComputationThen
   (do tokens   <- ExceptT $ readSource filename
       program  <- ExceptT $ return $ parse tokens
       ExceptT $ return $ inferKinds program 
       program' <- ExceptT $ return $ inferTypeWithMain program
       return program')
   (\program' -> putStrLn $ show program')

runDesugaring :: String -> IO ()
runDesugaring filename = 
  runComputationThen
   (do tokens   <- ExceptT $ readSource filename
       program  <- ExceptT $ return $ parse tokens
       ExceptT $ return $ inferKinds program
       program' <- ExceptT $ return $ inferTypeWithMain program
       termC    <- ExceptT $ return $ desugarProgram program'
       return termC)
   (\termC -> putStrLn $ show termC)

runEvaluator :: String -> IO ()
runEvaluator filename = 
  runComputationThen
   (do tokens               <- ExceptT $ readSource filename
       (program, namespace) <- ExceptT $ return $ parseAndGetNamespace tokens 
       ExceptT $ return $ inferKinds program
       program'             <- ExceptT $ return $ inferTypeWithMain program
       termC                <- ExceptT $ return $ desugarProgram program'
       return (namespace, termC))
   (\(namespace, termC) -> evalInNamespace namespace termC)

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

