
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
    testProgramOK "Simple Integer" (unlines [
      "main : Int",
      "main = 1"
    ]),

    testProgramError "failing case with wrong signature" (unlines [
      "data Bool where",
      "  true : Bool",
      "main : Bool",
      "main = 1"
    ]) TypeErrorUnificationClash,

    testProgramError "failing case with wrong Body" (unlines [
      "data Bool where",
      "  true : Bool",
      "main : Int",
      "main = true"
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

    testProgramOK "Lambda with simple data structures" (unlines [
      "data Bool where { True : Bool }",
      "main = (\\ True -> True) True"
    ]),

    testProgramOK "Lambda with complex use of data structures and pattern matching" (unlines [
      "data Bool where",
      " true : Bool",
      "data List a where",
      " nil  : List a",
      " cons : a -> List a -> List a",
      "main : Bool",
      "main = (\\ (cons x xs) -> x) (cons true nil)"
    ]),

    testProgramError "Lambda with wrong function type definintion" (unlines [
      "data Bool where",
      " true : Bool",
      "data List a where",
      " nil  : List a",
      " cons : a -> List a -> List a",
      "main : List Bool",
      "main = (\\ (cons x xs) -> x) (cons true nil)"
    ]) TypeErrorUnificationClash,

    testProgramError "Lambda with wrong uses of data structures" (unlines [
      "data Bool where { True : Bool }",
      "data Unit where { unit : Unit }",
      "main = (\\ unit -> True) True"
    ]) TypeErrorUnificationClash,

    testProgramError "lambda with a unbounded variable" (unlines [
      "main = \\ x -> y"
    ]) TypeErrorUnboundVariable
  ],

  TestSuite "Let" [
    testProgramOK "Simple let" (unlines [
      "data Bool where",
      " true : Bool",
      "main = let x = true in x"
    ]),

    testProgramOK "Let defining two let variables" (unlines [
      "data Bool where",
      " true : Bool",
      " false : Bool",
      "main : Bool",
      "main = let x = true ; y = false in x"
    ]),

    testProgramOK "Let with function declaration" (unlines [
      "data Bool where",
      " true : Bool",
      " false : Bool",
      "main : Bool",
      "main = let f x = x ; x = true in f x"
    ]),

    testProgramOK "Let with explicit polymorphic declaration" (unlines [
      "data Bool where true : Bool",
      "data Unit where unit : Unit",
      "main = let f : a -> Bool",
      "           f x = true",
      "        in f (f unit)"
    ]),

    -- TODO: THIS ONE SHOULD BE UNCOMMENTED WHEN WE FINISH THE INFERING SYSTEM
    -- testProgramOK "Let with implicit polymorphic declaration" (unlines [
    --   "data Bool where true : Bool",
    --   "data Unit where unit : Unit",
    --   "main = let f x = true",
    --   "        in f (f unit)"
    -- ]),

    testProgramError "Let with non declared variable" (unlines [
      "main = (let f x = x in f) f"
    ]) TypeErrorUnboundVariable
  ],

    TestSuite "Case of" [
    testProgramOK "Empty case" (unlines [
      "main x = case x of"
    ]),

    testProgramOK "Simple case with simple branch" (unlines [
      "main x = case x of x -> x"
    ]),

    testProgramOK "Case with boolean pattern matching" (unlines [
      "data Bool where",
      " true : Bool",
      " false : Bool",
      "data List a where",
      " nil  : List a",
      " cons : a -> List a -> List a",
      "main x = case x of",
      " false   -> true",
      " a   -> a"
    ]),

    testProgramOK "Case with list pattern matching" (unlines [
      "data Bool where",
      " true : Bool",
      " false : Bool",
      "data List a where",
      " nil  : List a",
      " cons : a -> List a -> List a",
      "main x = case x of",
      " (cons y ys)   -> false",
      " nil   -> true"
    ]),

    testProgramError "Case when the type of the patterns don't match" (unlines [
      "data Bool where",
      " true : Bool",
      " false : Bool",
      "data List a where",
      " nil  : List a",
      " cons : a -> List a -> List a",
      "main x = case x of",
      " (cons y ys)   -> false",
      " true   -> true"
    ]) TypeErrorUnificationClash,

    testProgramError "Case when the type of the case's bodies don't match" (unlines [
      "data Bool where",
      " true : Bool",
      " false : Bool",
      "data List a where",
      " nil  : List a",
      " cons : a -> List a -> List a",
      "main x = case x of",
      " (cons y ys)   -> nil",
      " nil   -> true"
    ]) TypeErrorUnificationClash
  ],

  TestSuite "Fresh" [
    testProgramOK "Simple fresh variable" (unlines [
      "main = fresh x in x"
    ]),

    testProgramOK "Defining fresh function" (unlines [
      "data Pair a b where { pair : a -> b -> Pair a b }",
      "main = fresh f in pair (f 1) (f 2)"
    ]),

    testProgramError "Fail case" (unlines [
      "main = fresh x in x x"
    ]) TypeErrorUnificationOccursCheck,

    testProgramError "Fail fresh function" (unlines [
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

