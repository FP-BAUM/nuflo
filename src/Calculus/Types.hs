module Calculus.Types(
         TypeMetavariable, TypeConstraint(..),
         TypeScheme(..),  ConstrainedType(..), Type(..),
         substituteConstrainedType,
         constrainedTypeFreeVariables,
         typeSchemeMetavariables,
         tFun, tInt
       ) where

import Syntax.Name(QName(..), operatorArrow, primitiveInt)
import qualified Data.Set as S
import qualified Data.Map as M

type TypeMetavariable = Integer

data TypeConstraint = TypeConstraint QName Type
  deriving Eq

data TypeScheme = TypeScheme [QName] ConstrainedType

data ConstrainedType = ConstrainedType [TypeConstraint] Type
  deriving Eq

data Type = TMetavar TypeMetavariable
          | TVar QName
          | TApp Type Type
          deriving Eq

type TypeSubstitution = M.Map QName Type

---- Metavariables

typeSchemeMetavariables :: TypeScheme -> S.Set TypeMetavariable
typeSchemeMetavariables (TypeScheme _ ctype) =
  constrainedTypeMetavariables ctype

constrainedTypeMetavariables :: ConstrainedType -> S.Set TypeMetavariable
constrainedTypeMetavariables (ConstrainedType typeConstraints typ) =
  S.unions (map typeConstraintMetavariables typeConstraints)
  `S.union`
  typeMetavariables typ

typeConstraintMetavariables :: TypeConstraint -> S.Set TypeMetavariable
typeConstraintMetavariables (TypeConstraint _ typ) = typeMetavariables typ

typeMetavariables :: Type -> S.Set TypeMetavariable
typeMetavariables (TMetavar metaVar) = S.singleton metaVar
typeMetavariables (TVar name)        = S.empty
typeMetavariables (TApp t1 t2)       = typeMetavariables t1 `S.union`
                                       typeMetavariables t2

---- Free variables

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
typeFreeVariables (TApp typ1 typ2) =
  typeFreeVariables typ1 `S.union` typeFreeVariables typ2

---- Apply a substitution

substituteConstraint :: TypeSubstitution -> TypeConstraint -> TypeConstraint
substituteConstraint sub (TypeConstraint cls typ) =
  TypeConstraint cls (substituteType sub typ)

substituteType :: TypeSubstitution -> Type -> Type
substituteType sub t@(TVar name)  = M.findWithDefault t name sub
substituteType sub (TApp t1 t2)   = TApp (substituteType sub t1)
                                         (substituteType sub t2)
substituteType _   t@(TMetavar _) = t

substituteConstrainedType :: TypeSubstitution -> ConstrainedType
                          -> ConstrainedType
substituteConstrainedType sub (ConstrainedType constraints typ) =
  ConstrainedType (map (substituteConstraint sub) constraints)
                  (substituteType sub typ)

---- Type constants

tFun :: Type -> Type -> Type
tFun = TApp . TApp (TVar operatorArrow)

tInt :: Type
tInt = TVar primitiveInt

---- Show

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

