module Calculus.Terms(Term(..), Program(..), Location) where

import Syntax.Name(QName(..))

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
          deriving Show

data Program = Fail
             | Alt Term Program
  deriving Show

type Location = Integer

