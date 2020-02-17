
module TestParser(tests) where

import Test(TestSuite(..), Test(..))

import Error(Error(..), ErrorType(..))
import Lexer.Lexer(tokenize)
import Lexer.Name(QName(..))
import Parser.AST(Program(..), AnnDeclaration(..), AnnExpr(..), Expr,
                  eraseAnnotations)
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

tests :: TestSuite
tests = TestSuite "PARSER" [
  testProgramError "Expect module name after module keyword"
     "module module" 
     ParseError,

  -- Expressions
  testExprOK "Variable"
     "x = y" 
     (EVar () (qmain "y")),

  testExprOK "Integer constant"
     "x = 42" 
     (EInt () 42),

  --TODO: qualified

  -- Empty program
  testProgramOK "Empty program"
    "" 
    Program {
      programDeclarations = []
    }
  ]

