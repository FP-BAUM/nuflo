module Calculus.Terms(
         PrimitiveFunction(..),
         PrimitiveCommand(..),
         Term(..), Program(..), Location, consOk, lam,
         applySubst, freeVariables, splitArgs
       ) where

import qualified Data.Map as M
import qualified Data.Set as S

import Syntax.Name(QName(..), primitiveOk)

data PrimitiveFunction = IntAdd
  deriving Show

data PrimitiveCommand = Print
  deriving Show

data Term = Var QName
          | Cons QName
          | ConstInt Integer
          | ConstChar Char
          | Fresh QName Term
          | Lam QName Program
          | LamL Location QName Program
          | Fix QName Term
          | App Term Term
          | Seq Term Term
          | Unif Term Term
          | Function PrimitiveFunction [Term]
          | Command PrimitiveCommand [Term]
          deriving Show

data Program = Fail
             | Alt Term Program
  deriving Show

type Location = Integer

consOk :: Term
consOk = Cons primitiveOk

lam :: QName -> Term -> Term
lam x t = Lam x (Alt t Fail)

applySubst :: M.Map QName Term -> Term -> Term
applySubst subst (Var x)       = M.findWithDefault (Var x) x subst
applySubst subst (Cons c)      = Cons c
applySubst subst (ConstInt n)  = ConstInt n
applySubst subst (ConstChar n) = ConstChar n
applySubst subst (Fresh x t) =
  Fresh x $ applySubst (M.insert x (Var x) subst) t
applySubst subst (Lam x p) =
  Lam x $ applySubstP (M.insert x (Var x) subst) p
applySubst subst (LamL l x p) =
  LamL l x $ applySubstP (M.insert x (Var x) subst) p
applySubst subst (Fix x t) =
  Fix x $ applySubst (M.insert x (Var x) subst) t
applySubst subst (App t1 t2) =
  App (applySubst subst t1) (applySubst subst t2)
applySubst subst (Seq t1 t2) =
  Seq (applySubst subst t1) (applySubst subst t2)
applySubst subst (Unif t1 t2) =
  Unif (applySubst subst t1) (applySubst subst t2)
applySubst subst (Function f ts) =
  Function f (map (applySubst subst) ts)
applySubst subst (Command f ts) =
  Command f (map (applySubst subst) ts)

applySubstP :: M.Map QName Term -> Program -> Program
applySubstP subst Fail      = Fail
applySubstP subst (Alt t p) = Alt (applySubst subst t)
                                      (applySubstP subst p)

freeVariables :: Term -> S.Set QName
freeVariables (Var name)          = S.singleton name
freeVariables (Cons name)         = S.empty
freeVariables (ConstInt _)        = S.empty
freeVariables (ConstChar _)       = S.empty
freeVariables (Fresh name t)      = freeVariables t S.\\ S.singleton name
freeVariables (Lam name p)        = freeVariablesP p S.\\ S.singleton name
freeVariables (LamL loc name p)   = freeVariablesP p S.\\ S.singleton name
freeVariables (Fix name t)        = freeVariables t S.\\ S.singleton name
freeVariables (App t1 t2)         = freeVariables t1 `S.union` freeVariables t2
freeVariables (Seq t1 t2)         = freeVariables t1 `S.union` freeVariables t2
freeVariables (Unif t1 t2)        = freeVariables t1 `S.union` freeVariables t2
freeVariables (Function _ terms)  = S.unions $ map freeVariables terms
freeVariables (Command _ terms)   = S.unions $ map freeVariables terms

freeVariablesP :: Program -> S.Set QName
freeVariablesP Fail = S.empty
freeVariablesP (Alt t p) = freeVariables t `S.union` freeVariablesP p

splitArgs :: Term -> (Term, [Term])
splitArgs (App t s) = let (head, args) = splitArgs t
                         in (head, args ++ [s])
splitArgs t         = (t, [])

