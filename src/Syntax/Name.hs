
module Syntax.Name(
         splitParts, allNameParts,
         isWellFormedName, isWellFormedOperatorName,
         QName(..), readName, qualify, moduleNameFromQName, unqualifiedName,
         modulePRIM, moduleMain, arrowSymbol,
         primitiveArrow, primitiveUnit, primitiveInt,
         primitiveAlternative, primitiveTuple, primitiveUnderscore,
         primitivePrint, primitiveOk
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

allNameParts :: String -> [String]
allNameParts id = id : (filter (/= "_") (splitParts id))

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

-- A well-formed name is moreover a well-formed operator name if
-- it has at least one underscore.
isWellFormedOperatorName :: String -> Bool
isWellFormedOperatorName s =
     isWellFormedName s
  && s /= "_" 
  && "_" `elem` splitParts s

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

moduleNameFromQName :: QName -> QName
moduleNameFromQName (Name id) =
  error ("Name \"" ++ id ++ "\" is unqualified.")
moduleNameFromQName (Qualified id (Name _)) = Name id
moduleNameFromQName (Qualified id qname)    =
  Qualified id (moduleNameFromQName qname)

unqualifiedName :: QName -> String
unqualifiedName (Name id)           = id
unqualifiedName (Qualified _ qname) = unqualifiedName qname

----

modulePRIM :: QName
modulePRIM = Name "PRIM"

moduleMain :: QName
moduleMain = Name "Main"

arrowSymbol :: String
arrowSymbol = "→"

primitiveArrow :: QName
primitiveArrow = qualify modulePRIM ("_" ++ arrowSymbol ++ "_")

primitiveUnit :: QName
primitiveUnit = qualify modulePRIM "()"

primitiveInt :: QName
primitiveInt = qualify modulePRIM "Int"

primitiveAlternative :: QName
primitiveAlternative = qualify modulePRIM ("_<>_")

primitiveTuple :: QName
primitiveTuple = qualify modulePRIM "{Tuple}"

primitiveUnderscore :: QName
primitiveUnderscore = qualify modulePRIM "_"

primitivePrint :: QName
primitivePrint = qualify modulePRIM "print"

primitiveOk :: QName
primitiveOk = qualify modulePRIM "ok"

