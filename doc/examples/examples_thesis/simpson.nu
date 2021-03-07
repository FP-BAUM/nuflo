data Simpson where
  Abe    : Simpson
  Homero : Simpson
  Bart   : Simpson
  Lisa   : Simpson
  Maggie : Simpson

father : Simpson -> Simpson                    -- Using pattern matching
father Homero = Abe
father Bart   = Homero
father Lisa   = Homero
father Maggie = Homero
-- father (Bart | Lisa | Maggie) = Homero      -- Optional, using pattern matching (internal unificacion mechanism) of the alternative operator

_∘_ f g x = f (g x)                            -- Function composition as an infix operator

child (father x) = x                           -- Child using pattern matching about father relation

inv : (a -> b) -> b -> a                       -- Inverse function
inv f (f x) = x                                -- Doing pattern matching about the application of the function f

grandparent = father ∘ father
grandson = inv grandparent

main () = print (grandson Abe) end           -- Print all of Abe's grandchildren
