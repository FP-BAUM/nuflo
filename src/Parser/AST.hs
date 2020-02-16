{-# LANGUAGE DuplicateRecordFields #-}
module Parser.AST(
         Program(..),
         AnnDeclaration(..), Declaration,
         AnnExpr(..), Expr,
         eraseAnnotations
       ) where

import Position(Position)
import Lexer.Name(QName)

data Program = Program {
                 programDeclarations :: [Declaration]
               }
  deriving (Eq, Show)

-- Annotated declaration
data AnnDeclaration a = 
    DataDeclaration {
      annotation       :: a,
      dataTypeName     :: AnnExpr a,
      dataConstructors :: [AnnConstructorDeclaration a]
    }
  | TypeDeclaration {
      annotation :: a,
      typeName   :: AnnExpr a,
      typeValue  :: AnnExpr a
    }
  | TypeSignature {
      annotation    :: a,
      signatureName :: QName,
      signatureType :: AnnExpr a
    }
  | ValueDeclaration {
      annotation :: a,
      declLHS  :: AnnExpr a,
      declRHS  :: AnnExpr a
    }
  deriving (Eq, Show)

data AnnConstructorDeclaration a = 
  ConstructorDeclaration {
    annotation      :: a,
    constructorName :: QName,
    constructorType :: AnnExpr a
  }
  deriving (Eq, Show)

-- Annotated expression
data AnnExpr a =
    EVar a QName                      -- variable
  | EApp a (AnnExpr a) (AnnExpr a)    -- application
  deriving (Eq, Show)

type Declaration            = AnnDeclaration Position
type ConstructorDeclaration = AnnConstructorDeclaration Position
type Expr                   = AnnExpr Position

--

class EraseAnnotations f where
  eraseAnnotations :: f a -> f ()

instance EraseAnnotations AnnExpr where
  eraseAnnotations (EVar _ q)     = EVar () q
  --eraseAnnotations (EApp _ e1 e2) = EApp () (eraseAnnotations e1)
  --                                          (eraseAnnotations e2)
  --TODO

