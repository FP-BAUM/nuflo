data Simpson where
  Abe    : Simpson
  Homero : Simpson
  Bart   : Simpson
  Lisa   : Simpson
  Maggie : Simpson

padre : Simpson -> Simpson                     -- Trivial, usando pattern matching
padre Homero = Abe
padre Bart   = Homero
padre Lisa   = Homero
padre Maggie = Homero
-- padre (Bart | Lisa | Maggie) = Homero       -- Opcional, usando pattern matching de la alternativa

_∘_ f g x = f (g x)                            -- Composición de funciones como un operador infijo

hijo (padre x) = x                             -- Definición de hijo haciendo pattern matching en la función padre

inv : (a -> b) -> b -> a                       -- Definición de función inversa
inv f (f x) = x                                -- Es definida haciendo pattern matching en la aplicación de la función f

main () = print (inv (padre ∘ padre) Abe) end  -- Imprime todos los nietos de Abe
