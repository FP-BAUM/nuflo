
module Lexer.Name(isWellFormedName) where

import Lexer.Categories(isKeyword, isInteger)

splitParts :: String -> [String]
splitParts ""         = []
splitParts [x]        = [[x]]
splitParts ('_' : xs) = "_" : splitParts xs
splitParts (x : xs)   = let (p : ps) = splitParts xs in
                          if p == "_"
                           then [x] : p : ps
                           else (x : p) : ps

-- A name part is well-formed if it is not a keyword nor an integer.
isWellFormedNamePart :: String -> Bool
isWellFormedNamePart s = not (isKeyword s) && not (isInteger s)

-- A name is well-formed if:
-- - It has at least one part.
-- - All of its parts are well-formed.
-- - Underscores alternate with non-underscores.
isWellFormedName :: String -> Bool
isWellFormedName s = case splitParts s of
                       []       -> False
                       (p : ps) -> isWellFormedNamePart p &&
                                   rec (p == "_") ps
  where
    rec _ []       = True
    rec b (p : ps) = isWellFormedNamePart p &&
                     b /= (p == "_") &&
                     rec (not b) ps

