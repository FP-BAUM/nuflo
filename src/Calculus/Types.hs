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

typeConstraintFreeVariable :: TypeConstraint -> QName
typeConstraintFreeVariable (TypeConstraint _ name) = name

constrainedTypeFreeVariables :: ConstrainedType -> S.Set QName
constrainedTypeFreeVariables (ConstrainedType typeConstraints typ) =
  S.fromList (map typeConstraintFreeVariable typeConstraints)
  `S.union`
  typeFreeVariables typ

typeFreeVariables :: Type -> S.Set QName
typeFreeVariables (TMetavar _) = S.empty
typeFreeVariables (TVar name) = S.fromList [name]
typeFreeVariables (TApp typ1 typ2) =
  typeFreeVariables typ1 `S.union` typeFreeVariables typ2

----

joinS :: String -> [String] -> String
joinS _   []       = ""
joinS _   [l]      = l
joinS sep (l : ls) = l ++ sep ++ joinS sep ls

instance Show TypeConstraint where
  show (TypeConstraint cls typ) = show cls ++ " " ++ show typ

instance Show TypeScheme where
  show (TypeScheme vs ctyp) =
    (if null vs
      then ""
      else "forall {" ++ joinS "; " (map show vs) ++ "} ")
    ++
    show ctyp

instance Show ConstrainedType where
  show (ConstrainedType cs typ) =
    show typ ++ if null cs
                 then ""
                 else "{" ++ joinS "; " (map show cs) ++ "}"

instance Show Type where
  show (TMetavar n) = "?" ++ show n
  show (TVar a)     = show a
  show (TApp t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

