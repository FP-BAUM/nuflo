module Calculus.Types(
         TypeMetavariable, TypeConstraint(..),
         TypeScheme(..),  ConstrainedType(..), Type(..),
         constrainedTypeFreeVariables
       ) where

import Syntax.Name(QName)
import qualified Data.Set as S

type TypeMetavariable = Integer

data TypeConstraint = TypeConstraint QName QName

data TypeScheme = TypeScheme [QName] ConstrainedType

data ConstrainedType = ConstrainedType [TypeConstraint] Type

data Type = TMetavar TypeMetavariable
          | TVar QName
          | TApp Type Type


typeFreeVariables :: Type -> (S.Set QName)
typeFreeVariables (TMetavar _) = S.empty
typeFreeVariables (TVar name) = S.fromList [name]
typeFreeVariables (TApp typ1 typ2) = typeFreeVariables typ1 `S.union` (typeFreeVariables typ2)

typeConstraintFreeVariable :: TypeConstraint -> QName
typeConstraintFreeVariable (TypeConstraint _ name) = name

constrainedTypeFreeVariables :: ConstrainedType -> (S.Set QName)
constrainedTypeFreeVariables (ConstrainedType typeConstraints typ) =
  S.fromList (map typeConstraintFreeVariable typeConstraints) `S.union` typeFreeVariables typ
