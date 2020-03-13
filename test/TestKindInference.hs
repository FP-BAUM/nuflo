
module TestKindInference(tests) where

import Test(Test(..))

import Error(Error(..), ErrorType(..))
import Lexer.Lexer(tokenize)
import Parser.Parser(parse)
import Infer.KindInference(inferKinds)

testProgram :: String -> String -> Either ErrorType () -> Test
testProgram description source expected =
  TestCase description 
           (normalizeResult
             (return source >>= tokenize "test"
                            >>= parse
                            >>= inferKinds))
           expected
  where 
    normalizeResult (Left  e) = Left (errorType e)
    normalizeResult (Right x) = Right x

testProgramOK :: String -> String -> Test
testProgramOK description source = testProgram description source (Right ())

testProgramError :: String -> String -> ErrorType -> Test
testProgramError description source expected =
  testProgram description source (Left expected)

----

tests :: Test
tests = TestSuite "KIND INFERENCE" [
  testProgramOK "Empty program" ""
 ]

