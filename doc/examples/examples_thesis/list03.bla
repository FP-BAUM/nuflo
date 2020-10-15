_++_ : List a -> List a -> List a
[] ++ ys       = ys
(x : xs) ++ ys = x : (xs ++ ys)

prefijo : List a -> List a
prefijo (xs ++ ys) = xs

last : List a -> a
last (_ ++ (x : [])) = x

foldr : (a -> b -> b) -> b -> List a -> b
foldr _ z []       = z
foldr f z (x : xs) = f x (foldr f z xs)

inter : a -> List a -> List a
inter x []       = x : []
inter x (y : ys) = x : (y : ys)
inter x (y : ys) = y : inter x ys

main () = print (inter 1 (1 : 2 : 3 : [])) end