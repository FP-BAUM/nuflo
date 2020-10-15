_++_ : List a -> List a -> List a
[] ++ ys       = ys
(x : xs) ++ ys = x : (xs ++ ys)

prefijo : List a -> List a
prefijo (xs ++ ys) = xs

last : List a -> a
last (_ ++ (x : [])) = x


main () = print (last (1 : 2 : 3 : [])) end