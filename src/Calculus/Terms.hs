module Calculus.Terms(
         Builtin(..), Term(..), Program(..), Location, consOk, lam
       ) where

import Syntax.Name(QName(..), primitiveOk)

data Builtin = Print
  deriving Show

builtinArity :: Builtin -> Integer
builtinArity Print = 1

data Term = Var QName
          | Cons QName
          | Num Integer
          | Fresh QName Term
          | Lam QName Program
          | LamL Location QName Program
          | Fix QName Term
          | App Term Term
          | Seq Term Term
          | Unif Term Term
          | Primitive Builtin [Term]
          deriving Show

data Program = Fail
             | Alt Term Program
  deriving Show

type Location = Integer

consOk :: Term
consOk = Cons primitiveOk

lam :: QName -> Term -> Term
lam x t = Lam x (Alt t Fail)

