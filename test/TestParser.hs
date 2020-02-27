
module TestParser(tests) where

import Test(TestSuite(..), Test(..))

import Error(Error(..), ErrorType(..))
import Syntax.Name(QName(..))
import Syntax.AST(Program(..), AnnDeclaration(..), AnnExpr(..), Expr,
                  eraseAnnotations)
import Lexer.Lexer(tokenize)
import Parser.Parser(parse)

testProgram :: String -> String -> Either ErrorType Program -> Test
testProgram description source expected =
  TestCase description 
           (normalizeResult (tokenize "test" source >>= parse))
           expected
  where
    normalizeResult (Left  e) = Left (errorType e)
    normalizeResult (Right x) = Right x

testProgramOK :: String -> String -> Program -> Test
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
eapp = foldl (EApp ())

tests :: TestSuite
tests = TestSuite "PARSER" [
  testProgramError "Expect module name after module keyword"
     "module module" 
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
       "infixl 20 foo_",
       "x = foo"
     ])
     ParseError,

  testExprOK "Basic prefix operator"
     (unlines [
       "infixl 20 a_",
       "x = a 10"
     ])
     (eapp (evar "a_") [eint 10]),

  testExprOK "Basic suffix operator"
     (unlines [
       "infixl 20 _!",
       "x = 10 !"
     ])
     (eapp (evar "_!") [eint 10]),

  testExprOK "Basic infix operator"
     (unlines [
       "infixl 20 _++_",
       "x = 10 ++ 20"
     ])
     (eapp (evar "_++_") [eint 10, eint 20]),

  testExprOK "Basic circumfix operator"
     (unlines [
       "infixl 20 [[_]]",
       "x = [[ z ]]"
     ])
     (eapp (evar "[[_]]") [evar "z"]),

  testExprOK "Nested infix operators"
     (unlines [
       "infixl 20 _+_",
       "infixl 30 _*_",
       "infixl 40 _^_",
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
       "infixl 20 _+_",
       "infixl 30 _*_",
       "infixl 40 _^_",
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
       "infixl 20 foo_bar_baz_",
       "infixl 30 _!!",
       "infixl 40 ??_",
       "x = ??(foo ?? 1 bar ?? 2 !! baz 3 !!)"
     ])
     (eapp (evar "??_") [
        eapp (evar "foo_bar_baz_") [
          eapp (evar "??_") [eint 1],
          eapp (evar "_!!") [eapp (evar "??_") [eint 2]],
          eapp (evar "_!!") [eint 3]
        ]
      ]),

  --TODO: qualified

  -- Empty program
  testProgramOK "Empty program"
    "" 
    Program {
      programDeclarations = []
    }
  ]

