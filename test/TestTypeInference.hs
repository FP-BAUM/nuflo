
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

  TestSuite "Int" [

    testProgramOK "Simple integer" (unlines [
      "main : Int",
      "main = 1"
    ]),

    testProgramError "Reject non-int declared as int" (unlines [
      "data Bool where",
      "  true : Bool",
      "main : Int",
      "main = true"
    ]) TypeErrorUnificationClash,

    testProgramError "Reject int declared as non-int" (unlines [
      "data Bool where",
      "  true : Bool",
      "main : Bool",
      "main = 1"
    ]) TypeErrorUnificationClash
  ],

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

  TestSuite "Lambda" [

    testProgramOK "Simple Lambda" (unlines [
      "main = \\ x -> x"
    ]),

    testProgramOK "Pattern matching - constant constructor" (unlines [
      "data Bool where { True : Bool }",
      "main = (\\ True -> True) True"
    ]),

    testProgramOK "Pattern matching - binding" (unlines [
      "data Bool where",
      " true : Bool",
      "data List a where",
      " nil  : List a",
      " cons : a -> List a -> List a",
      "main : Bool",
      "main = (\\ (cons x xs) -> x) (cons true nil)"
    ]),

    testProgramError "Match type with arrow" (unlines [
      "data Bool where",
      " true : Bool",
      "data List a where",
      " nil  : List a",
      " cons : a -> List a -> List a",
      "main : List Bool",
      "main = (\\ (cons x xs) -> x) (cons true nil)"
    ]) TypeErrorUnificationClash,

    testProgramError "Match type of argument" (unlines [
      "data Bool where { True : Bool }",
      "data Unit where { unit : Unit }",
      "main = (\\ unit -> True) True"
    ]) TypeErrorUnificationClash,

    testProgramError "Reject unbound variable" (unlines [
      "main = \\ x -> y"
    ]) TypeErrorUnboundVariable
  ],

  TestSuite "Let" [
    testProgramOK "Simple let" (unlines [
      "data Bool where",
      " true : Bool",
      "main = let x = true in x"
    ]),

    testProgramOK "Allow many definitions" (unlines [
      "data Bool where",
      " true : Bool",
      " false : Bool",
      "main : Bool",
      "main = let x = true ; y = false in x"
    ]),

    testProgramOK "Allow function definitions" (unlines [
      "data Bool where",
      " true : Bool",
      " false : Bool",
      "main : Bool",
      "main = let f x = x ; x = true in f x"
    ]),

    testProgramOK "Explicit polymorphic declaration" (unlines [
      "data Bool where true : Bool",
      "data Unit where unit : Unit",
      "main = let f : a -> Bool",
      "           f x = true",
      "        in f (f unit)"
    ]),

    testProgramOK "Implicit polymorphic declaration" (unlines [
      "data Bool where true : Bool",
      "data Unit where unit : Unit",
      "main = let f x = true",
      "        in f (f unit)"
    ]),

    testProgramError "Exit scope after let" (unlines [
      "main = (let f x = x in f) f"
    ]) TypeErrorUnboundVariable
  ],

  TestSuite "Case" [
    testProgramOK "Empty case" (unlines [
      "main x = case x of"
    ]),

    testProgramOK "Simple case" (unlines [
      "main x = case x of x -> x"
    ]),

    testProgramOK "Non-binding pattern matching" (unlines [
      "data Bool where",
      " true : Bool",
      " false : Bool",
      "main x = case x of",
      " false -> true",
      " a     -> a"
    ]),

    testProgramOK "Binding pattern matching" (unlines [
      "data Bool where",
      " true : Bool",
      " false : Bool",
      "data List a where",
      " nil  : List a",
      " cons : a -> List a -> List a",
      "main x = case x of",
      " (cons y ys) -> false",
      " nil         -> true"
    ]),

    testProgramError "Reject patterns of different types" (unlines [
      "data Bool where",
      " true : Bool",
      " false : Bool",
      "data List a where",
      " nil  : List a",
      " cons : a -> List a -> List a",
      "main x = case x of",
      " (cons y ys) -> false",
      " true        -> true"
    ]) TypeErrorUnificationClash,

    testProgramError "Reject branches of different types" (unlines [
      "data Bool where",
      " true : Bool",
      " false : Bool",
      "data List a where",
      " nil  : List a",
      " cons : a -> List a -> List a",
      "main x = case x of",
      " (cons y ys) -> nil",
      " nil         -> true"
    ]) TypeErrorUnificationClash
  ],

  TestSuite "Fresh" [
    testProgramOK "Simple fresh variable" (unlines [
      "main = fresh x in x"
    ]),

    testProgramOK "Fresh function" (unlines [
      "data Pair a b where { pair : a -> b -> Pair a b }",
      "main = fresh f in pair (f 1) (f 2)"
    ]),

    testProgramError "Reject fresh variable (occurs check)" (unlines [
      "main = fresh x in x x"
    ]) TypeErrorUnificationOccursCheck,

    testProgramError "Reject fresh variable (clashing types)" (unlines [
      "data Bool where { true : Bool }",
      "data Pair a b where { pair : a -> b -> Pair a b }",
      "main = fresh f in pair (f 1) (f true)"
    ]) TypeErrorUnificationClash
  ],

  testProgramError "Reject unbound variable" (unlines [
    "f x = y"
  ]) TypeErrorUnboundVariable,

  testProgramOK "Empty program" ""

 ]

