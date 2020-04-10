
module TestTypeInference(tests) where

import Test(Test(..))

import Error(Error(..), ErrorType(..))
import Lexer.Lexer(tokenize)
import Parser.Parser(parse)
import Infer.KindInference(inferKinds)
import Infer.TypeInference(inferTypes)

testProgram :: String -> String -> Either ErrorType () -> Test
testProgram description source expected =
  TestCase description 
           (normalizeResult
             (do tokens <- tokenize "test" source
                 ast    <- parse tokens
                 inferKinds ast
                 inferTypes ast))
           expected
  where 
    normalizeResult (Left  e) = Left (errorType e)
    normalizeResult (Right _) = Right ()

testProgramOK :: String -> String -> Test
testProgramOK description source = testProgram description source (Right ())

testProgramError :: String -> String -> ErrorType -> Test
testProgramError description source expected =
  testProgram description source (Left expected)

----

tests :: Test
tests = TestSuite "TYPE INFERENCE" [

  testProgramOK "Basic data declaration" (unlines [
    "data Bool where",
    "  True : Bool",
    "  False : Bool"
  ]),

  testProgramError "Reject repeated constructors" (unlines [
    "data Bool where",
    "  True : Bool",
    "  True : Bool"
  ]) TypeErrorVariableAlreadyDeclared,

  testProgramError "Reject repeated signatures" (unlines [
    "f : a -> b -> c",
    "f : b -> a -> c"
  ]) TypeErrorVariableAlreadyDeclared,

  testProgramOK "Check simple function definition" (unlines [
    "f x = x"
  ]),

  TestSuite "Application" [

    testProgramOK "Simple application" (unlines [
      "x = x",
      "f = f",
      "main = f x"
    ]),

    testProgramOK "Nested application" (unlines [
      "x = x",
      "f = f",
      "g = g",
      "main = f (f x)"
    ]),

    testProgramError "Occurs check failure" (unlines [
      "f = f",
      "main = f f"
    ]) TypeErrorUnificationOccursCheck,

    testProgramOK "Application with type declaration" (unlines [
      "data Bool where",
      "  True : Bool",
      "f : Bool -> Bool",
      "f = f",
      "main = f (f True)"
    ]),

    testProgramError "Application with type declaration - clash" (unlines [
      "data Unit where",
      "  unit : Unit",
      "data Bool where",
      "  True : Bool",
      "f : Bool -> Unit",
      "f = f",
      "main = f (f True)"
    ]) TypeErrorUnificationClash

  ],

  testProgramError "Reject unbound variable" (unlines [
    "f x = y"
  ]) TypeErrorUnboundVariable,

  testProgramOK "Empty program" ""

 ]

