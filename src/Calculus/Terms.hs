module Calculus.Terms(
         PrimitiveFunction(..),
         PrimitiveCommand(..),
         Term(..), Program(..), Location, consOk, lam
       ) where

import Syntax.Name(QName(..), primitiveOk)

data PrimitiveFunction = IntAdd
  deriving Show

data PrimitiveCommand = Print
  deriving Show

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

