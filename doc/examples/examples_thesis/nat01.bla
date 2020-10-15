data Nat where
  Z : Nat
  S  : Nat -> Nat

_+_ : Nat -> Nat -> Nat
Z + n = n
S n + m = S (n + m)

main () = print (
            fresh x in
              (S (S Z) + x) ~ S (S (S Z))
              & x
          ) end
