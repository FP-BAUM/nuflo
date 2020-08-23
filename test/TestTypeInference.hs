
module TestTypeInference(tests) where

import Test(Test(..))

import Error(Error(..), ErrorType(..))
import Lexer.Lexer(tokenize)
import Parser.Parser(parse)
import Infer.KindInference(inferKinds)
import Infer.TypeInference(inferTypeWithoutMain)

testProgram :: String -> String -> Either ErrorType () -> Test
testProgram description source expected =
  TestCase description 
           (normalizeResult
             (do tokens <- tokenize "test" source
                 ast    <- parse tokens
                 inferKinds ast
                 inferTypeWithoutMain ast))
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
      "start : Int",
      "start = 1"
    ]),

    testProgramError "Reject non-int declared as int" (unlines [
      "data Bool where",
      "  true : Bool",
      "start : Int",
      "start = true"
    ]) TypeErrorUnificationClash,

    testProgramError "Reject int declared as non-int" (unlines [
      "data Bool where",
      "  true : Bool",
      "start : Bool",
      "start = 1"
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
      "start = f x"
    ]),

    testProgramOK "Nested application" (unlines [
      "x = x",
      "f = f",
      "g = g",
      "start = f (f x)"
    ]),

    testProgramOK "Self application of polymorphic function" (unlines [
      "f = f",
      "start = f f"
    ]),

    testProgramOK "Application with type declaration" (unlines [
      "data Bool where",
      "  True : Bool",
      "f : Bool -> Bool",
      "f = f",
      "start = f (f True)"
    ]),

    testProgramError "Application with type declaration - clash" (unlines [
      "data Unit where",
      "  unit : Unit",
      "data Bool where",
      "  True : Bool",
      "f : Bool -> Unit",
      "f = f",
      "start = f (f True)"
    ]) TypeErrorUnificationClash

  ],

  TestSuite "Lambda" [

    testProgramOK "Simple Lambda" (unlines [
      "start = \\ x -> x"
    ]),

    testProgramOK "Pattern matching - constant constructor" (unlines [
      "data Bool where { True : Bool }",
      "start = (\\ True -> True) True"
    ]),

    testProgramOK "Pattern matching - binding" (unlines [
      "data Bool where",
      " true : Bool",
      "data List a where",
      " nil  : List a",
      " cons : a -> List a -> List a",
      "start : Bool",
      "start = (\\ (cons x xs) -> x) (cons true nil)"
    ]),

    testProgramError "Match type with arrow" (unlines [
      "data Bool where",
      " true : Bool",
      "data List a where",
      " nil  : List a",
      " cons : a -> List a -> List a",
      "start : List Bool",
      "start = (\\ (cons x xs) -> x) (cons true nil)"
    ]) TypeErrorUnificationClash,

    testProgramError "Match type of argument" (unlines [
      "data Bool where { True : Bool }",
      "data Unit where { unit : Unit }",
      "start = (\\ unit -> True) True"
    ]) TypeErrorUnificationClash,

    testProgramError "Reject unbound variable" (unlines [
      "start = \\ x -> y"
    ]) TypeErrorUnboundVariable
  ],

  TestSuite "Let" [
    testProgramOK "Simple let" (unlines [
      "data Bool where",
      " true : Bool",
      "start = let x = true in x"
    ]),

    testProgramOK "Allow many definitions" (unlines [
      "data Bool where",
      " true : Bool",
      " false : Bool",
      "start : Bool",
      "start = let x = true ; y = false in x"
    ]),

    testProgramOK "Allow function definitions" (unlines [
      "data Bool where",
      " true : Bool",
      " false : Bool",
      "start : Bool",
      "start = let f (.x) = x ; x = true in f x"
    ]),

    testProgramOK "Explicit polymorphic declaration" (unlines [
      "data Bool where true : Bool",
      "data Unit where unit : Unit",
      "start = let f : a -> Bool",
      "            f x = true",
      "         in f (f unit)"
    ]),

    testProgramOK "Implicit polymorphic declaration" (unlines [
      "data Bool where true : Bool",
      "data Unit where unit : Unit",
      "start = let f x = true",
      "         in f (f unit)"
    ]),

    testProgramError "Disallow instantiation of rigid variable" (unlines [
      "start = let f : a -> a",
      "            f 1 = 2",
      "         in 3"
    ]) TypeErrorUnificationClash,

    testProgramError "Exit scope after let" (unlines [
      "start = (let f x = x in f) f"
    ]) TypeErrorUnboundVariable
  ],

  TestSuite "Case" [
    testProgramOK "Empty case" (unlines [
      "start x = case x of"
    ]),

    testProgramOK "Simple case" (unlines [
      "start x = case x of x -> x"
    ]),

    testProgramOK "Non-binding pattern matching" (unlines [
      "data Bool where",
      " true : Bool",
      " false : Bool",
      "start x = case x of",
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
      "start x = case x of",
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
      "start x = case x of",
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
      "start x = case x of",
      " (cons y ys) -> nil",
      " nil         -> true"
    ]) TypeErrorUnificationClash
  ],

  TestSuite "Fresh" [
    testProgramOK "Simple fresh variable" (unlines [
      "start = fresh x in x"
    ]),

    testProgramOK "Fresh function" (unlines [
      "data Pair a b where { pair : a -> b -> Pair a b }",
      "start = fresh f in pair (f 1) (f 2)"
    ]),

    testProgramError "Reject fresh variable (occurs check)" (unlines [
      "start = fresh x in x x"
    ]) TypeErrorUnificationOccursCheck,

    testProgramError "Reject fresh variable (clashing types)" (unlines [
      "data Bool where { true : Bool }",
      "data Pair a b where { pair : a -> b -> Pair a b }",
      "start = fresh f in pair (f 1) (f true)"
    ]) TypeErrorUnificationClash
  ],

  TestSuite "Explicitly unbound variables" [

    testProgramError "Lambda does not bind already bound variable" (unlines [
      "data Bool where",
      "  true : Bool",
      "start : Bool -> Bool",
      "start x = (\\ x -> true) 1"
    ]) TypeErrorUnificationClash,

    testProgramOK "Lambda binds explicitly unbound variable" (unlines [
      "data Bool where",
      "  true : Bool",
      "start : Bool -> Bool",
      "start x = (\\ .x -> true) 1"
    ]),

    testProgramError "Case does not bind already bound variable" (unlines [
      "data Bool where",
      "  true : Bool",
      "start : Bool -> Bool",
      "start x = case 1 of { x -> true }"
    ]) TypeErrorUnificationClash,

    testProgramOK "Case binds explicitly unbound variable" (unlines [
      "data Bool where",
      "  true : Bool",
      "start : Bool -> Bool",
      "start x = case 1 of { .x -> true }"
    ])

  ],

  TestSuite "Type synonyms" [

    testProgramOK "Type synonym without parameters" (unlines [
      "type I = Int",
      "type II = I -> I",
      "a : I",
      "a = 3",
      "z : II",
      "z x = a",
      "f : II -> II",
      "f h n = h (z 10)"
    ]),

    testProgramError "Type synonym without parameters - fail" (unlines [
      "data Bool where { true : Bool }",
      "type I = Int",
      "type II = I -> I",
      "a : I",
      "a = 3",
      "z : II",
      "z x = a",
      "f : II -> II",
      "f h n = h (z true)"
    ]) TypeErrorUnificationClash,

    testProgramOK "Type synonym with parameters" (unlines [
      "type FF f a = f a -> f a",
      "type GG a = a -> a",
      "f : FF GG Int",
      "f h n = h n"
    ]),

    testProgramOK "Partially applied type synonym" (unlines [
      "data List a where { nil : List a ; cons : a -> List a -> List a }",
      "data Bool where { true : Bool }",
      "type FF f a b = f b -> f a",
      "f : FF List Int Bool",
      "f (cons true nil) = cons 1 nil"
    ]),

    testProgramError "Partially applied type synonym - fail" (unlines [
      "data List a where { nil : List a ; cons : a -> List a -> List a }",
      "data Bool where { true : Bool }",
      "type FF f a b = f b -> f a",
      "f : FF List Int Bool",
      "f (cons 1 nil) = cons 1 nil"
    ]) TypeErrorUnificationClash,

    TestSuite "Reject loops" [
      testProgramError "Reject simple loop" (unlines [
        "type A = A",
        "start : A",
        "start = 1"
      ]) TypeErrorSynonymLoop,

      testProgramError "Reject indirect loop" (unlines [
        "type A = B",
        "type B = C",
        "type C = A",
        "start : B",
        "start = 1"
      ]) TypeErrorSynonymLoop,

      testProgramError "Reject loop in functions" (unlines [
        "type F a b = G a",
        "type G a = F a a",
        "start : F Int Int",
        "start = 1"
      ]) TypeErrorSynonymLoop
    ]

  ],

  TestSuite "Class declarations" [

    testProgramOK "Emtpy class declaration" (unlines [
      "class Eq a where"
    ]),

    testProgramError "Reject constrained class parameter" (unlines [
      "class Show a where",
      "class Eq a where",
      " f : a -> b {Show a}"
    ]) ClassErrorConstrainedParameter,

    testProgramOK "Use class method" (unlines [
      "class F a where",
      " f : a -> a",
      "start : a -> a  {F a}",
      "start x = f x"
    ]),

    testProgramOK "Use class method before declaration" (unlines [
      "start : a -> a  {F a}",
      "start x = f x",
      "class F a where",
      " f : a -> a"
    ])
  ],

  TestSuite "Instance declarations" [

    testProgramOK "Basic instance declaration" (unlines [
      "data Bool where",
      "  True  : Bool",
      "  False : Bool",
      "class Eq a where",
      "  == : a -> a -> Bool",
      "instance Eq Bool where",
      "  == True  True  = True",
      "  == True  False = False",
      "  == False True  = False",
      "  == False False = True"
    ]),

    testProgramError "Reject duplicate instance declarations" (unlines [
      "data Bool where",
      "  True  : Bool",
      "class Eq a where",
      "  == : a -> a -> Bool",
      "instance Eq Bool where",
      "  == True  True  = True",
      "instance Eq Bool where",
      "  == True  False = True"
    ]) InstanceErrorDuplicateInstance,

    testProgramOK "Accept method declarations in different order" (unlines [
      "data Bool where",
      "  True  : Bool",
      "  False : Bool",
      "class F a where",
      "  f : a",
      "  g : a",
      "instance F Bool where",
      "  g = True",
      "  f = True",
      "  f = False"
    ]),

    testProgramOK "Instance declaration with parameter" (unlines [
      "data Bool where",
      "  True  : Bool",
      "  False : Bool",
      "data List a where",
      "  Nil   : List a",
      "  Cons  : a -> List a -> List a",
      "class Eq a where",
      "  == : a -> a -> Bool",
      "instance Eq Bool where",
      "  == x y = True",
      "instance Eq (List a) where",
      "  == Nil         Nil         = True",
      "  == (Cons x xs) (Cons y ys) = True"
    ]),

    testProgramError "Reject instance declaration missing methods" (unlines [
      "data Bool where",
      "  True  : Bool",
      "  False : Bool",
      "class F a where",
      "  f : a",
      "  g : a",
      "instance F Bool where",
      "  g = True"
    ]) InstanceErrorMethodMismatch,

    testProgramError "Reject duplicated method definitions" (unlines [
      "data Bool where",
      "  True  : Bool",
      "  False : Bool",
      "class F a where",
      "  f : a",
      "  g : a",
      "instance F Bool where",
      "  f = True",
      "  g = True",
      "  f = False"
    ]) InstanceErrorDuplicatedMethodDefinition,

    testProgramError "Reject definition of method not declared in class"
      (unlines [
        "data Bool where",
        "  True  : Bool",
        "  False : Bool",
        "class F a where",
        "  f : a",
        "  g : a",
        "instance F Bool where",
        "  f = True",
        "  g = True",
        "  h = True"
      ]) TypeErrorUndefinedClassMethod

  ],

  TestSuite "Constraint solving" [

    testProgramOK "Instantiate metavariable in constrained metavariable"
      (unlines [
        "class F a where",
        "  f : a -> a -> Bool",
        "start : a -> Bool {F a}",
        "start y = (let x : a {F a}",
        "               x = x",
        "            in f x) y"
      ]),

    testProgramError "Fail upon unresolved instance placeholder" (unlines [
      "class F a where",
      " f : a -> a",
      "start = f"
    ]) ConstraintErrorUnresolvedPlaceholder,

    testProgramError "Unsolvable constraint (undeclared instance)" (unlines [
      "class F a where",
      "  f : a -> a",
      "start = f 1"
    ]) ConstraintErrorUndeclaredInstance

  ],
  
  testProgramError "Reject unbound variable" (unlines [
    "f x = y"
  ]) TypeErrorUnboundVariable,

  testProgramOK "Empty program" ""

 ]

