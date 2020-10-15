_++_ : List a -> List a -> List a
[] ++ ys       = ys
(x : xs) ++ ys = x : (xs ++ ys)

prefijo : List a -> List a
prefijo (xs ++ ys) = xs

main () = print (prefijo (1 : 2 : 3 : [])) end