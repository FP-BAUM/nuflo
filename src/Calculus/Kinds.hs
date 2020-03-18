
module Calculus.Kinds(Kind(..), KindVariable, kindIn) where

type KindVariable = Integer

data Kind = KVar KindVariable
          | KType
          | KFun Kind Kind

kindIn :: KindVariable -> Kind -> Bool
kindIn x (KVar y)     = x == y
kindIn x KType        = False
kindIn x (KFun k1 k2) = kindIn x k1 || kindIn x k2

instance Show Kind where
  show (KVar n)     = "?" ++ show n
  show KType        = "*"
  show (KFun k1 k2) = pshow k1 ++ " → " ++ show k2
    where
      pshow k@(KFun _ _) = "(" ++ show k ++ ")"
      pshow k            = show k

