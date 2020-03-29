module Calculus.Types(
         TypeMetavariable, TypeConstraint(..),
         TypeScheme(..),  ConstrainedType(..), Type(..),
         substituteContrainedType,
         constrainedTypeFreeVariables
       ) where

import Syntax.Name(QName)
import qualified Data.Set as S
import qualified Data.Map as M

type TypeMetavariable = Integer

data TypeConstraint = TypeConstraint QName Type

data TypeScheme = TypeScheme [QName] ConstrainedType

data ConstrainedType = ConstrainedType [TypeConstraint] Type

data Type = TMetavar TypeMetavariable
          | TVar QName
          | TApp Type Type

typeConstraintFreeVariables :: TypeConstraint -> S.Set QName
typeConstraintFreeVariables (TypeConstraint _ typ) = typeFreeVariables typ

constrainedTypeFreeVariables :: ConstrainedType -> S.Set QName
constrainedTypeFreeVariables (ConstrainedType typeConstraints typ) =
  S.unions (map typeConstraintFreeVariables typeConstraints)
  `S.union`
  typeFreeVariables typ

typeFreeVariables :: Type -> S.Set QName
typeFreeVariables (TMetavar _) = S.empty
typeFreeVariables (TVar name) = S.fromList [name]
typeFreeVariables (TApp typ1 typ2) = typeFreeVariables typ1 `S.union` typeFreeVariables typ2

substituteConstraint :: M.Map QName Type -> TypeConstraint -> TypeConstraint
substituteConstraint dict (TypeConstraint cls typ) = TypeConstraint cls (substituteType dict typ)

substituteType :: M.Map QName Type -> Type -> Type
substituteType dict t@(TVar name)  = (M.findWithDefault t name dict)
substituteType dict (TApp t1 t2)   = TApp (substituteType dict t1) (substituteType dict t2)
substituteType _ t@(TMetavar meta) = t

substituteContrainedType :: M.Map QName Type -> ConstrainedType -> ConstrainedType
substituteContrainedType dict (ConstrainedType constraints typ) = let constraints'  = map (substituteConstraint dict) constraints
                                                                      typ'          = substituteType dict typ
                                                                  in ConstrainedType constraints' typ'

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

