
module Lexer.Name(
         isWellFormedName, isWellFormedOperatorName,
         QName(..), readName, qualify, unqualifiedName
       ) where

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

isWellFormedOperatorName :: String -> Bool
isWellFormedOperatorName s = isWellFormedName s && "_" `elem` splitParts s

-- Qualified name
data QName =
    Name String
  | Qualified String QName
  deriving (Eq, Ord)

instance Show QName where
  show (Name s)        = s
  show (Qualified q s) = q ++ "." ++ show s

readName :: String -> QName
readName s
  | isWellFormedName s = Name s
  | otherwise          = error "Name is not well-formed."

qualify :: QName -> String -> QName
qualify (Name id)            id' = Qualified id (readName id')
qualify (Qualified id qname) id' = Qualified id (qualify qname id')

unqualifiedName :: QName -> String
unqualifiedName (Name id)           = id
unqualifiedName (Qualified _ qname) = unqualifiedName qname

