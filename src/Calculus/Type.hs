module Calculus.Types(ConstrainedType(..)) where

import Syntax.Name(QName)

type TypeMetavariable = Integer

data TypeConstraint = TypeConstraint {
  constraintClassName :: QName,
  constraintTypeName  :: QName
}

data ConstrainedType = CT [TypeConstraint] Type

data Type = TMetavar TypeMetavariable
          | TForall TypeMetavariable Type
          | TVar QName
          | TApp Type Type

