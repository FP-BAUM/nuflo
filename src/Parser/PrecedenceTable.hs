module Parser.PrecedenceTable(
         PrecedenceTable, PrecedenceLevel, Associativity(..),
         Precedence, emptyPrecedenceTable, precedenceLevel,
         declareOperator, precedenceTableLevels, isOperator,
         isOperatorPart, operatorFixity
       ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List((\\))

import Error(ErrorMessage(..))
import Syntax.Name(
         QName, splitParts, isWellFormedOperatorName, qualify,
         moduleNameFromQName, unqualifiedName
       )

data Associativity = RightAssoc | LeftAssoc | NonAssoc
  deriving (Show, Eq, Ord)

data PrecedenceTable = PT {
      ptPrecedenceLevels :: M.Map Precedence PrecedenceLevel,
      ptAssociativities  :: M.Map QName Associativity,
      ptAllOperatorParts :: S.Set QName
    }
  deriving (Show, Eq)

type Precedence = Integer

type PrecedenceLevel = S.Set QName

emptyPrecedenceTable :: PrecedenceTable
emptyPrecedenceTable = PT {
                         ptPrecedenceLevels = M.empty,
                         ptAssociativities  = M.empty,
                         ptAllOperatorParts = S.empty
                       }

precedenceLevel :: Precedence -> PrecedenceTable -> PrecedenceLevel
precedenceLevel precedence table =
  M.findWithDefault S.empty precedence (ptPrecedenceLevels table)

precedenceTableLevels :: PrecedenceTable -> [PrecedenceLevel]
precedenceTableLevels table = map snd (M.toAscList (ptPrecedenceLevels table))

addOperator :: Associativity -> Precedence -> QName -> PrecedenceTable
            -> PrecedenceTable
addOperator assoc precedence qname table =
  let level = precedenceLevel precedence table
    in PT {
         ptPrecedenceLevels =
           M.insert precedence (S.insert qname level)
                    (ptPrecedenceLevels table),
         ptAssociativities =
           M.insert qname assoc (ptAssociativities table),
         ptAllOperatorParts =
           ptAllOperatorParts table `S.union`
           S.fromList (
             map (qualify (moduleNameFromQName qname))
                 (filter (/= "_") (splitParts (unqualifiedName qname)))
           )
       }

declareOperator :: Associativity -> Precedence -> QName -> PrecedenceTable
                -> Either ErrorMessage PrecedenceTable
declareOperator assoc precedence qname table = 
  if isWellFormedOperatorName (unqualifiedName qname)
   then Right $ addOperator assoc precedence qname table
   else Left ("\"" ++ show qname ++ "\" is not a valid operator name.")

isOperator :: QName -> PrecedenceTable -> Bool
isOperator qname table = M.member qname (ptAssociativities table)

isOperatorPart :: QName -> PrecedenceTable -> Bool
isOperatorPart qname table = S.member qname (ptAllOperatorParts table)

operatorFixity :: QName -> PrecedenceTable -> Associativity
operatorFixity op table =
  M.findWithDefault
    (error ("Undefined operator \"" ++ show op ++ "\"."))
    op
    (ptAssociativities table)

