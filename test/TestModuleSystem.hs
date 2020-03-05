
module TestModuleSystem(tests) where

import Test(TestSuite(..), Test(..))

import Error(Error(..), ErrorType(..))
import Syntax.Name(QName(..))
import Syntax.AST(AnnProgram(..), AnnDeclaration(..),
                  AnnEquation(..), AnnExpr(..), Expr,
                  eraseAnnotations)
import Lexer.Lexer(tokenize)
import Parser.Parser(parse)

testExpr :: String -> String -> Either ErrorType (AnnExpr ()) -> Test
testExpr description source expected =
  TestCase description 
           (normalizeResult (tokenize "test" source >>= parse))
           expected
  where
    normalizeResult (Left  e) = Left (errorType e)
    normalizeResult (Right p) =
      Right (eraseAnnotations
              (equationRHS
                (declEquation (last (programDeclarations p)))))

testExprOK :: String -> String -> AnnExpr () -> Test
testExprOK description source expected =
  testExpr description source (Right expected)

testExprError :: String -> String -> ErrorType -> Test
testExprError description source expected =
  testExpr description source (Left expected)

----

tests :: TestSuite
tests = TestSuite "MODULE SYSTEM" [
  testExprOK "Export all / import all"
     "module A where { a = 1 } module B where { import A; x = a }" 
     (EVar () (Qualified "A" (Name "a"))),

  testExprOK "Export all / import name"
     "module A where { a = 1 } module B where { import A(a); x = a }" 
     (EVar () (Qualified "A" (Name "a"))),

  testExprOK "Export all / do not import"
     "module A where { a = 1 } module B where { import A(); x = a }" 
     (EVar () (Qualified "B" (Name "a"))),

  testExprOK "Export all / import renaming (use)"
     "module A where { a = 1 } module B where { import A(a as z); x = a }" 
     (EVar () (Qualified "B" (Name "a"))),

  testExprOK "Export all / import renaming (do not use)"
     "module A where { a = 1 } module B where { import A(a as z); x = z }" 
     (EVar () (Qualified "A" (Name "a"))),

  testExprOK "Export all / import qualified"
     "module A where { a = 1 } module B where { import A(); x = A.a }" 
     (EVar () (Qualified "A" (Name "a"))),

  testExprOK "Do not export / import all"
     "module A() where { a = 1 } module B where { import A; x = a }" 
     (EVar () (Qualified "B" (Name "a"))),

  testExprError "Do not export / import name"
     "module A() where { a = 1 } module B where { import A(a); x = a }" 
     ModuleSystemError,

  testExprError "Do not export / import renaming (use)"
     "module A() where { a = 1 } module B where { import A(a as z); x = z }" 
     ModuleSystemError,

  testExprError "Do not export / import qualified"
     "module A() where { a = 1 } module B where { import A(); x = A.a }" 
     ModuleSystemError,

  testExprOK "Export name / import all"
     "module A(a) where { a = 1 } module B where { import A; x = a }" 
     (EVar () (Qualified "A" (Name "a"))),

  testExprOK "Export name / import name"
     "module A(a) where { a = 1 } module B where { import A(a); x = a }" 
     (EVar () (Qualified "A" (Name "a"))),

  testExprOK "Export name / do not import"
     "module A(a) where { a = 1 } module B where { import A(); x = a }" 
     (EVar () (Qualified "B" (Name "a"))),

  testExprOK "Export name / import renaming (use)"
     "module A(a) where { a = 1 } module B where { import A(a as z); x = z }" 
     (EVar () (Qualified "A" (Name "a"))),

  testExprOK "Export name / import renaming (do not use)"
     "module A(a) where { a = 1 } module B where { import A(a as z); x = a }" 
     (EVar () (Qualified "B" (Name "a"))),

  testExprOK "Export name / import qualified"
     "module A(a) where { a = 1 } module B where { import A(); x = A.a }" 
     (EVar () (Qualified "A" (Name "a"))),

  testExprError "Import non-existing module"
     "module B where { import A; x = 1 }" 
     ModuleSystemError,

  testExprError "Import non-existing name from module"
     "module A where {} module B where { import A(a); x = 1 }" 
     ModuleSystemError,

  testExprError "Ambiguous name"
     "module A where { x = 1 } module B where { x = 2; import A; y = x }" 
     ModuleSystemError,

  -- Operator parts

  testExprOK "Export operator / do not import operator part"
     (unlines [
        "module A where { infix 20 foo_ }",
        "module B where { x = foo }" 
      ])
     (EVar () (Qualified "B" (Name "foo"))),

  testExprOK "Export operator / import operator part"
     (unlines [
        "module A where { infix 20 foo_ }",
        "module B where { import A; x = foo 3 }" 
      ])
     (EApp ()
        (EVar () (Qualified "A" (Name "foo_")))
        (EInt () 3)),

  testExprOK "Export operator explicitly / import operator part"
     (unlines [
        "module A(foo_) where { infix 20 foo_ }",
        "module B where { import A; x = foo 3 }" 
      ])
     (EApp ()
        (EVar () (Qualified "A" (Name "foo_")))
        (EInt () 3)),

  testExprOK "Export operator / import operator explicitly part"
     (unlines [
        "module A where { infix 20 foo_ }",
        "module B where { import A(foo_); x = foo 3 }" 
      ])
     (EApp ()
        (EVar () (Qualified "A" (Name "foo_")))
        (EInt () 3)),

  testExprError "Cannot rename operator on import"
     (unlines [
        "module A where { infix 20 if_then_else_ }",
        "module B where { import A(if_then_else_ as uf_then_ulse_); x = if }" 
      ])
     ModuleSystemError

  ]

