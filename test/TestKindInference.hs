
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
             (do tokens <- tokenize "test" source
                 ast    <- parse tokens
                 inferKinds ast))
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

  testProgramOK "Data declaration" (unlines [
    "data Map k v where"
  ]),

  testProgramError "Malformed type in data declaration" (unlines [
    "data Map (f k) v where"
  ]) KindErrorMalformedDatatype,

  testProgramOK "Type declaration" (unlines [
    "data Empty where",
    "type Void = Empty",
    "type Const a b = a"
  ]),

  testProgramError "Malformed type in type declaration" (unlines [
    "data Empty where",
    "type Void (f x) = Empty"
  ]) KindErrorMalformedDatatype,

  testProgramError "Type cannot be redeclared" (unlines [
    "data Empty where",
    "data Bool where",
    "type Bool = Empty"
  ]) KindErrorTypeAlreadyDeclared,

  testProgramError "Reject repeated parameters" (unlines [
    "data Map a a where"
  ]) KindErrorTypeAlreadyDeclared,

  testProgramOK "Accept constructors returning datatype" (unlines [
    "data Map a b where",
    "  empty  : Map a b",
    "  insert : a → b → Map a b → Map a b"
  ]),

  testProgramError "Reject constructors not returning datatype" (unlines [
    "data Map a b where",
    "  empty  : Map a b",
    "  insert : a → b → Map a b → Map b a"
  ]) KindErrorConstructorShouldReturnDataType,

  testProgramOK "Allow type signature of base kind (*)" (unlines [
    "data List a where",
    "x : Int",
    "y : List Int",
    "z : List (List Int)",
    "f : Int → List (List Int) → List Int"
  ]),

  testProgramError "Reject type signature with malformed type" (unlines [
    "x : let z = 1 in z"
  ]) KindErrorMalformedType,

  testProgramError "Reject type signature not of base kind (*)" (unlines [
    "data List a where",
    "x : List"
  ]) KindErrorUnifyClash,

  testProgramError "Reject type signature by occurs check (1)" (unlines [
    "data A f where",
    "  B : f f → A f"
  ]) KindErrorUnifyOccursCheck,

  testProgramError "Reject type signature by occurs check (2)" (unlines [
    "data A f a where",
    "  B : f a → a f → A f a"
  ]) KindErrorUnifyOccursCheck,

  testProgramOK "Allow free variables in signatures" (unlines [
    "data List a where",
    "  []   : List a",
    "  _::_ : a → List a → List a",
    "map : (a → b) → List a → List b"
  ]),

  testProgramOK "Check kinds of type declarations (1)" (unlines [
    "data List a where",
    "type LL a = List (List a)",
    "x : LL (LL Int)",
    "y : LL (LL (Int → Int) → LL (Int → Int))"
  ]),

  testProgramError "Check kinds of type declarations (2)" (unlines [
    "data List a where",
    "type LL a = List (List a)",
    "x : LL (LL (_→_ Int))"
  ]) KindErrorUnifyClash,

  testProgramOK "Check kinds of type declarations (3)" (unlines [
    "data F a b where",
    "data G a where",
    "type H a b = F (G a) (G b)",
    "x : H (_→_ Int) (_→_ Int)"
  ]),

  testProgramOK "Check kinds of type declarations (4)" (unlines [
    "data F a b where",
    "data G a where",
    "type H a b = F (G a) (G b)",
    "x : H Int Int"
  ]),

  testProgramError "Check kinds of type declarations (5)" (unlines [
    "data F a b where",
    "data G a where",
    "type H a b = F (G a) (G b)",
    "x : H Int Int",
    "y : H (_→_ Int) (_→_ Int)"
  ]) KindErrorUnifyClash,

  testProgramError "Check kinds of type declarations (6)" (unlines [
    "data F a b where",
    "data G a where",
    "type H a b = F (G a) (G b)",
    "x : H Int (_→_ Int)"
  ]) KindErrorUnifyClash,

  testProgramError "Disallow two declarations of a class" (unlines [
    "class A a where",
    "class A a where"
  ]) KindErrorClassAlreadyDeclared,

  testProgramOK "Check kinds of class methods (1)" (unlines [
    "data Bool where",
    "class Eq a where",
    "  _==_ : a → a → Bool",
    "class Ret a where",
    "  return : b → a b"
  ]),

  testProgramError "Check kinds of class methods (2)" (unlines [
    "data Bool where",
    "class Eq a where",
    "  _==_ : a → a → Bool",
    "  return : b → a b"
  ]) KindErrorUnifyClash,

  testProgramError "Check kinds of constraints (1)" (unlines [
    "f : a → a → Bool   {Eq a}"
  ]) KindErrorClassUndeclared,

  testProgramError "Check kinds of constraints (2)" (unlines [
    "class Eq a where",
    "f : a → a → Bool   {Eq b}"
  ]) KindErrorTypeUndeclared,

  testProgramOK "Check kinds of constraints (3)" (unlines [
    "class Eq a where",
    "f : a → a → Bool   {Eq a}"
  ]),

  testProgramError "Check kinds of constraints (4)" (unlines [
    "data List a where",
    "  []  : List a",
    "  _∷_ : a → List a → List a",
    "class Monad m where",
    "  return : a → m a",
    "x : List m    {Monad m}"
  ]) KindErrorUnifyClash,

  testProgramOK "Check kinds of constraints (5)" (unlines [
    "data List a where",
    "  []  : List a",
    "  _∷_ : a → List a → List a",
    "class Monad m where",
    "  return : a → m a",
    "mapM : m (List a) → List (m a)   {Monad m}"
  ]),

  testProgramOK "Empty instance of empty class" (unlines [
    "data Empty where",
    "instance EmptyClass Empty where",
    "class EmptyClass a where"
  ]),

  testProgramError "Reject instance of undeclared class" (unlines [
    "data Bool where",
    "instance Eq Bool where"
  ]) KindErrorClassUndeclared,

  testProgramError "Reject instance of undeclared type" (unlines [
    "class EmptyClass a where",
    "instance EmptyClass Bool where"
  ]) KindErrorTypeUndeclared,

  testProgramError "Check instance kinds (1)" (unlines [
    "data Bool where",
    "data List a where",
    "  _∷_ : a → List a → List a",
    "class Eq a where",
    "  _==_ : a → a → Bool",
    "instance Eq List where"
  ]) KindErrorUnifyClash,

  testProgramOK "Check instance kinds (2)" (unlines [
    "data Bool where",
    "data List a where",
    "  _∷_ : a → List a → List a",
    "class Eq a where",
    "  _==_ : a → a → Bool",
    "instance Eq Bool where",
    "instance Eq (List a) {Eq a} where"
  ]),

  testProgramError "Check instance kinds (3)" (unlines [
    "data Bool where",
    "data List a where",
    "  _∷_ : a → List a → List a",
    "class Eq a where",
    "  _==_ : a → a → Bool",
    "instance Eq Bool where",
    "instance Eq (List a) {Eq List} where"
  ]) KindErrorUnifyClash,

  testProgramError "Reject instance declaration with synonym" (unlines [
    "data Bool where",
    "type BB = Bool",
    "class Eq a where",
    "instance Eq BB where"
  ]) KindErrorInstanceMustBeDatatype,

  testProgramError "Reject argument in instance declaration" (unlines [
    "data Bool where",
    "data List a where",
    "class Eq a where",
    "instance Eq (List Bool) where"
  ]) KindErrorInstanceArgCannotBeDatatype,

  testProgramError "Check kinds inside let/where" (unlines [
    "data List a where",
    "main = x where x : List"
  ]) KindErrorUnifyClash,

  testProgramOK "Empty program" ""

 ]

