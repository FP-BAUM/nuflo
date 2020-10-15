-- Inferidor de tipos para el calculo lambda simplemente tipado

data Id where
  Z : Id
  S  : Id -> Id

data Type where
  BOOL : Type
  _=>_ : Type -> Type -> Type

data Ctx where
  Ec : Ctx
  _,_ : Ctx -> Type -> Ctx

data Term where
  var : Id -> Term
  lam : Term -> Term
  app : Term -> Term -> Term

_Ê_::_ : Ctx -> Id -> Type -> ()
(gamma , A) Ê Z   :: A = ()
(gamma , A) Ê S x :: B = gamma Ê x :: B

_|-_::_ : Ctx -> Term -> Type -> ()
gamma |- var x   :: A = gamma Ê x :: A
gamma |- lam t   :: (A => B) = (gamma , A) |- t :: B
gamma |- app t s :: B = fresh A in
                          gamma |- t :: (A => B)
                        & gamma |- s :: A

main () = print (
                fresh A in
                  (Ec |- lam (var Z) :: A)
                & A
                ) end
