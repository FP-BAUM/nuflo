
module Parser.AST(
         Program(..), AnnDeclaration(..), Declaration, AnnExpr(..), Expr
       ) where

import Position(Position)
import Lexer.Name(QName)

data Program = Program {
                 programDeclarations :: [Declaration]
               }
  deriving (Eq, Show)

-- Annotated declaration
data AnnDeclaration a = 
    NameDeclaration a QName (AnnExpr a)
  deriving (Eq, Show)

-- Annotated expression
data AnnExpr a =
    Var a QName                    -- variable
  | App (AnnExpr a) (AnnExpr a)    -- application
  deriving (Eq, Show)

type Declaration = AnnDeclaration Position
type Expr = AnnExpr Position

