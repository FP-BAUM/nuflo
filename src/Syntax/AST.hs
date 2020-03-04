{-# LANGUAGE DuplicateRecordFields #-}

module Syntax.AST(
         AnnProgram(..), Program,
         AnnDeclaration(..), Declaration,
         AnnSignature(..), Signature,
         AnnExpr(..), Expr,
         eraseAnnotations, exprIsVariable, exprHeadVariable
       ) where

import Position(Position)
import Syntax.Name(QName)

data AnnProgram a = Program {
                      programDeclarations :: [AnnDeclaration a]
                    }
  deriving (Eq, Show)

type Program = AnnProgram Position

-- Annotated declaration
data AnnDeclaration a = 
    DataDeclaration {
      annotation       :: a,
      dataTypeName     :: AnnExpr a,
      dataConstructors :: [AnnSignature a]
    }
  | TypeDeclaration {
      annotation :: a,
      typeName   :: AnnExpr a,
      typeValue  :: AnnExpr a
    }
  | TypeSignature {
      typeSignature :: AnnSignature a
    }
  | ValueDeclaration {
      annotation :: a,
      declLHS  :: AnnExpr a,
      declRHS  :: AnnExpr a
    }
  | ClassDeclaration {
      annotation       :: a,
      className        :: QName,
      classTypeName    :: QName,
      classMethods     :: [AnnSignature a]
    }
  deriving (Eq, Show)

data AnnSignature a = Signature {
  annotation      :: a,
  signatureName   :: QName,
  signatureValue  :: AnnExpr a
} deriving (Eq, Show)

-- Annotated expression
data AnnExpr a =
    EVar a QName                      -- variable
  | EInt a Integer                    -- integer constant
  | EApp a (AnnExpr a) (AnnExpr a)    -- application
  deriving (Eq, Show)

type Declaration            = AnnDeclaration Position
type Signature              = AnnSignature Position
type Expr                   = AnnExpr Position

--

class EraseAnnotations f where
  eraseAnnotations :: f a -> f ()

instance EraseAnnotations AnnProgram where
  eraseAnnotations (Program x) = Program (map eraseAnnotations x)

instance EraseAnnotations AnnDeclaration where
  eraseAnnotations (DataDeclaration _ x y) =
    DataDeclaration () (eraseAnnotations x) (map eraseAnnotations y)
  eraseAnnotations (TypeDeclaration _ x y) =
    TypeDeclaration () (eraseAnnotations x) (eraseAnnotations y)
  eraseAnnotations (TypeSignature x) =
    TypeSignature $ eraseAnnotations x
  eraseAnnotations (ValueDeclaration _ x y) =
    ValueDeclaration () (eraseAnnotations x) (eraseAnnotations y)

instance EraseAnnotations AnnSignature where
  eraseAnnotations (Signature _ x y) =
    Signature () x (eraseAnnotations y)

instance EraseAnnotations AnnExpr where
  eraseAnnotations (EVar _ q)     = EVar () q
  eraseAnnotations (EInt _ n)     = EInt () n
  eraseAnnotations (EApp _ e1 e2) = EApp () (eraseAnnotations e1)
                                            (eraseAnnotations e2)

--

exprIsVariable :: AnnExpr a -> Bool
exprIsVariable (EVar _ _) = True
exprIsVariable _          = False

exprHeadVariable :: AnnExpr a -> Maybe QName
exprHeadVariable (EVar _ q)    = return q
exprHeadVariable (EApp _ e1 _) = exprHeadVariable e1
exprHeadVariable _             = Nothing

