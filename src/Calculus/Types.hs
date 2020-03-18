module Calculus.Types(
         TypeMetavariable, TypeConstraint(..),
         TypeScheme(..),  ConstrainedType(..), Type(..)
       ) where

import Syntax.Name(QName)

type TypeMetavariable = Integer

data TypeConstraint = TypeConstraint QName QName

data TypeScheme = TypeScheme [QName] ConstrainedType

data ConstrainedType = ConstrainedType [TypeConstraint] Type

data Type = TMetavar TypeMetavariable
          | TVar QName
          | TApp Type Type

