data Nat where
  Zero : Nat
  Suc  : Nat -> Nat

data Bool where
  True : Bool
  False : Bool

div : Nat -> Nat -> Nat
div 

isNotOne : Nat -> Bool
isNotOne (Suc Zero) = False
isNotOne _ = True

not : Bool
not True = False
not False = True

isEven : Nat -> Bool
isEven n = fresh x in Zero == (div n 2) & True <> False

isOdd : Nat -> Bool
isOdd n = not (isEven n)

nat : Nat
nat = Zero
nat = Suc nat

collatz_ = fresh x in x ~ nat & True ~ (collatz n) & n

collatz n = ((isOdd n) ~ True & (3 * n + 1)) <> ((isEven n) ~ True & (n / 2)) ; n != 1

main () = print collatz_