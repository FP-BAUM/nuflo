
module Syntax.Name(
         splitParts, allNameParts, nameArity,
         isWellFormedName, isWellFormedOperatorName,
         QName(..), readName, qualify, moduleNameFromQName, unqualifiedName,
         modulePRIM, moduleMain, arrowSymbol, colonSymbol,
         primitiveArrow, primitiveUnit,
         primitiveInt, primitiveChar,
         primitiveAlternative, primitiveSequence, primitiveUnification, 
         primitiveTuple, primitiveUnderscore,
         primitiveMain,
         primitivePrint,
         primitiveIO, primitiveEnd,
         primitivePut, primitiveGet, primitiveGetChar, primitiveGetLine,
         primitiveFail, primitiveOk,
         primitiveList, primitiveListNil, primitiveListCons
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

nameArity :: String -> Int
nameArity name = length (filter (== "_") (splitParts name))

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

colonSymbol :: String
colonSymbol = ":"

primitiveArrow :: QName
primitiveArrow = qualify modulePRIM ("_" ++ arrowSymbol ++ "_")

primitiveUnit :: QName
primitiveUnit = qualify modulePRIM "()"

primitiveInt :: QName
primitiveInt = qualify modulePRIM "Int"

primitiveChar :: QName
primitiveChar = qualify modulePRIM "Char"

primitiveAlternative :: QName
primitiveAlternative = qualify modulePRIM ("_|_")

primitiveSequence :: QName
primitiveSequence = qualify modulePRIM ("_&_")

primitiveUnification :: QName
primitiveUnification = qualify modulePRIM ("_~_")

primitiveTuple :: QName
primitiveTuple = qualify modulePRIM "{Tuple}"

primitiveUnderscore :: QName
primitiveUnderscore = qualify modulePRIM "_"

primitiveMain :: QName
primitiveMain = qualify modulePRIM "main"

primitivePrint :: QName
primitivePrint = qualify modulePRIM "print"

primitiveIO :: QName
primitiveIO = qualify modulePRIM "IO"

primitiveEnd :: QName
primitiveEnd = qualify modulePRIM "end"

primitivePut :: QName
primitivePut = qualify modulePRIM "put"

primitiveGet :: QName
primitiveGet = qualify modulePRIM "get"

primitiveGetChar :: QName
primitiveGetChar = qualify modulePRIM "getChar"

primitiveGetLine :: QName
primitiveGetLine = qualify modulePRIM "getLine"

primitiveFail :: QName
primitiveFail = qualify modulePRIM "fail"

primitiveOk :: QName
primitiveOk = qualify modulePRIM "ok"

primitiveList :: QName
primitiveList = qualify modulePRIM "List"

primitiveListNil :: QName
primitiveListNil = qualify modulePRIM "[]"

primitiveListCons :: QName
primitiveListCons = qualify modulePRIM "_:_"

