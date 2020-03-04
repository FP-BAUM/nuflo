
module TestParser(tests) where

import Test(TestSuite(..), Test(..))

import Error(Error(..), ErrorType(..))
import Syntax.Name(QName(..))
import Syntax.AST(AnnProgram(..), AnnDeclaration(..),
                  AnnSignature(..), Signature,
                  AnnConstraint(..), Constraint,
                  AnnExpr(..), Expr,
                  eraseAnnotations)
import Lexer.Lexer(tokenize)
import Parser.Parser(parse)

testProgram :: String -> String -> Either ErrorType (AnnProgram ()) -> Test
testProgram description source expected =
  TestCase description 
           (normalizeResult (tokenize "test" source >>= parse))
           expected
  where
    normalizeResult (Left  e) = Left (errorType e)
    normalizeResult (Right x) = Right (eraseAnnotations x)

testProgramOK :: String -> String -> AnnProgram () -> Test
testProgramOK description source expected =
  testProgram description source (Right expected)

testProgramError :: String -> String -> ErrorType -> Test
testProgramError description source expected =
  testProgram description source (Left expected)

----

testExpr :: String -> String -> Either ErrorType (AnnExpr ()) -> Test
testExpr description source expected =
  TestCase description 
           (normalizeResult (tokenize "test" source >>= parse))
           expected
  where
    normalizeResult (Left  e) = Left (errorType e)
    normalizeResult (Right p) =
      Right (eraseAnnotations (declRHS (last (programDeclarations p))))

testExprOK :: String -> String -> AnnExpr () -> Test
testExprOK description source expected =
  testExpr description source (Right expected)

testExprError :: String -> String -> ErrorType -> Test
testExprError description source expected =
  testExpr description source (Left expected)

----

qmain :: String -> QName
qmain x = Qualified "Main" (Name x)

evar :: String -> AnnExpr ()
evar x = EVar () (qmain x)

eint :: Integer -> AnnExpr ()
eint n = EInt () n

eapp :: AnnExpr () -> [AnnExpr ()] -> AnnExpr ()
eapp f []       = f
eapp f (x : xs) = eapp (EApp () f x) xs

tests :: TestSuite
tests = TestSuite "PARSER" [

  -- Declarations

  testProgramError "Expect module name after module keyword"
     "module module" 
     ParseError,

  testProgramOK "Single type declaration"
     (unlines [
       "type Name = String"
     ])
     (Program [
       TypeDeclaration ()
         (evar "Name")
         (evar "String")
     ]),

  testProgramOK "Type declaration"
     (unlines [
       "type Ages = List Integer"
     ])
     (Program [
       TypeDeclaration ()
         (evar "Ages")
         (eapp (evar "List") [evar "Integer"])
     ]),

  testProgramOK "Empty data declaration"
     (unlines [
       "data Empty where"
     ])
     (Program [
       DataDeclaration ()
         (evar "Empty")
         []
     ]),

  testProgramOK "Single data declaration"
     (unlines [
       "data Unit where",
       "  tt : Unit"
     ])
     (Program [
       DataDeclaration ()
         (evar "Unit")
         [
           Signature () (qmain "tt") (evar "Unit") []
         ]
     ]),

  testProgramOK "Data declaration"
     (unlines [
       "data Bool where",
       "  True  : Bool",
       "  False : Bool"
     ])
     (Program [
       DataDeclaration ()
         (evar "Bool")
         [
           Signature () (qmain "True") (evar "Bool") [],
           Signature () (qmain "False") (evar "Bool") []
         ]
     ]),

  testProgramOK "Empty class declaration"
     (unlines [
       "class Eq a where"
     ])
     (Program [
       ClassDeclaration ()
         (qmain "Eq")
         (qmain "a")
         []
     ]),

  testProgramOK "Class declaration with methods without constriants"
     (unlines [
       "class A b where",
       " f : a",
       " g : b"
     ])
     (Program [
       ClassDeclaration ()
         (qmain "A")
         (qmain "b") [
           Signature () (qmain "f") (evar "a") [],
           Signature () (qmain "g") (evar "b") []
         ]
     ]),

  testProgramOK "Class declaration with methods with constriants"
     (unlines [
       "class A b where",
       " f : a { Eq a ; Ord a }",
       " g : b { Ord b }"
     ])
     (Program [
       ClassDeclaration ()
         (qmain "A")
         (qmain "b") [
           Signature () (qmain "f") (evar "a") [
             Constraint () (qmain "Eq") (qmain "a"),
             Constraint () (qmain "Ord") (qmain "a")
           ],
           Signature () (qmain "g") (evar "b") [
             Constraint () (qmain "Ord") (qmain "b")
           ]
         ]
     ]),

  testProgramOK "Type signature with constraint"
     (unlines [
       "f : a { Eq a }"
     ])
     (Program [
       TypeSignature
         (Signature () (qmain "foo") (evar "a")
                       [Constraint () (qmain "Eq") (qmain "a")])
     ]),

  testProgramError "Invalid data declaration with no head variable"
     (unlines [
       "data 10 where"
     ])
     ParseError,

  testProgramError "Invalid type declaration with no head variable"
     (unlines [
       "type 10 = 10"
     ])
     ParseError,

  testProgramError "Invalid type signature with no head variable"
     (unlines [
       "data Bool where",
       "10 : Bool"
     ])
     ParseError,

  testProgramError "Invalid value declaration with no head variable"
     (unlines [
       "10 = 10"
     ])
     ParseError,

  -- Expressions

  testExprOK "Variable"
     "x = y" 
     (evar "y"),

  testExprOK "Integer constant"
     "x = 42" 
     (EInt () 42),

  -- Mixfix operators

  testExprError "Reject using operator part as variable"
     (unlines [
       "infix 20 foo_",
       "x = foo"
     ])
     ParseError,

  testExprOK "Basic prefix operator"
     (unlines [
       "infix 20 a_",
       "x = a 10"
     ])
     (eapp (evar "a_") [eint 10]),

  testExprOK "Basic suffix operator"
     (unlines [
       "infix 20 _!",
       "x = 10 !"
     ])
     (eapp (evar "_!") [eint 10]),

  testExprOK "Basic infix operator"
     (unlines [
       "infix 20 _++_",
       "x = 10 ++ 20"
     ])
     (eapp (evar "_++_") [eint 10, eint 20]),

  testExprOK "Basic circumfix operator"
     (unlines [
       "infix 20 [[_]]",
       "x = [[ z ]]"
     ])
     (eapp (evar "[[_]]") [evar "z"]),

  testExprOK "Nested infix operators"
     (unlines [
       "infix 20 _+_",
       "infix 30 _*_",
       "infix 40 _^_",
       "x = 1 ^ 2 * 3 ^ 4 + 5 ^ 6 * 7 ^ 8"
     ])
     (eapp (evar "_+_") [
       eapp (evar "_*_") [
         eapp (evar "_^_") [EInt () 1, EInt () 2],
         eapp (evar "_^_") [EInt () 3, EInt () 4]
       ],
       eapp (evar "_*_") [
         eapp (evar "_^_") [EInt () 5, EInt () 6],
         eapp (evar "_^_") [EInt () 7, EInt () 8]
       ]
     ]),

  testExprOK "Parentheses"
     (unlines [
       "infix 20 _+_",
       "infix 30 _*_",
       "infix 40 _^_",
       "x = ((1 + 2) * (3 + 4)) ^ ((5 + 6) * (7 + 8))"
     ])
     (eapp (evar "_^_") [
       eapp (evar "_*_") [
         eapp (evar "_+_") [EInt () 1, EInt () 2],
         eapp (evar "_+_") [EInt () 3, EInt () 4]
       ],
       eapp (evar "_*_") [
         eapp (evar "_+_") [EInt () 5, EInt () 6],
         eapp (evar "_+_") [EInt () 7, EInt () 8]
       ]
     ]),

  testExprOK "Mixed nested operators"
     (unlines [
       "infix 20 foo_bar_baz_",
       "infix 30 _!!",
       "infix 40 ??_",
       "x = ??(foo ?? 1 bar ?? 2 !! baz 3 !!)"
     ])
     (eapp (evar "??_") [
        eapp (evar "foo_bar_baz_") [
          eapp (evar "??_") [eint 1],
          eapp (evar "_!!") [eapp (evar "??_") [eint 2]],
          eapp (evar "_!!") [eint 3]
        ]
      ]),

  testExprError "Reject associating non-associative operator"
     (unlines [
       "infix 20 _+_",
       "x = 1 + 2 + 3"
     ])
     ParseError,

  testExprOK "Basic left-associative operator"
     (unlines [
       "infixl 20 _+_",
       "x = 1 + 2 + 3"
     ])
     (eapp (evar "_+_") [
        eapp (evar "_+_") [eint 1, eint 2],
        eint 3
      ]),

  testExprOK "Basic right-associative operator"
     (unlines [
       "infixr 20 _+_",
       "x = 1 + 2 + 3"
     ])
     (eapp (evar "_+_") [
        eint 1,
        eapp (evar "_+_") [eint 2, eint 3]
      ]),

  testExprOK "Application"
     (unlines [
       "x = f (g y) (h a b)"
     ])
     (eapp (evar "f") [
           (eapp (evar "g") [evar "y"]),
           (eapp (evar "h") [evar "a", evar "b"])
     ]),

  testExprOK "Import datatype from module"
     (unlines [
       "module A where",
       "  data Bool where",
       "    True : Bool",
       "module B where",
       "  import A",
       "  x = Bool"
     ])
     (EVar () (Qualified "A" (Name "Bool"))),

  testExprOK "Import constructor from module"
     (unlines [
       "module A where",
       "  data Bool where",
       "    True : Bool",
       "module B where",
       "  import A",
       "  x = True"
     ])
     (EVar () (Qualified "A" (Name "True"))),

  testExprOK "Import type from module"
     (unlines [
       "module A where",
       "  type N = Int",
       "module B where",
       "  import A",
       "  x = N"
     ])
     (EVar () (Qualified "A" (Name "N"))),

  -- Empty program
  testProgramOK "Empty program"
    "" 
    Program {
      programDeclarations = []
    }
  ]

