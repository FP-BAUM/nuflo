
module TestParser(tests) where

import Test(TestSuite(..), Test(..))

import Error(Error(..), ErrorType(..))
import Lexer.Lexer(tokenize)
import Parser.AST(Program(..))
import Parser.Parser(parse)

test :: String -> String -> Either ErrorType Program -> Test
test description source expectedResult =
  TestCase description 
           (normalizeResult (tokenize "test" source >>= parse))
           expectedResult
  where
    normalizeResult (Left  e) = Left (errorType e)
    normalizeResult (Right x) = Right x

testOK :: String -> String -> Program -> Test
testOK description source expected =
  test description source (Right expected)

testError :: String -> String -> ErrorType -> Test
testError description source expected =
  test description source (Left expected)

tests :: TestSuite
tests = TestSuite "PARSER" [

  testError "Expect module name after module keyword"
            "module module" 
            ParseError,

  -- Empty program
  testOK "Empty program"
         "" 
         Program {
           programDeclarations = []
         }
  ]

