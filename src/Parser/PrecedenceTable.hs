module Parser.PrecedenceTable(
         PrecedenceTable, PrecedenceLevel, Associativity(..),
         Precedence, Operator(..), emptyPrecedenceTable, precedenceLevel,
         declareOperator, precedenceTableLevels, isOperatorPart
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
                         ptLevels        :: M.Map Precedence PrecedenceLevel,
                         ptOperatorParts :: S.Set QName
                       }
  deriving (Show, Eq)

data Operator = Op QName Associativity
  deriving (Show, Eq, Ord)

type Precedence = Integer

type PrecedenceLevel = S.Set Operator

emptyPrecedenceTable :: PrecedenceTable
emptyPrecedenceTable = PT {
                         ptLevels        = M.empty,
                         ptOperatorParts = S.empty
                       }

precedenceLevel :: Precedence -> PrecedenceTable -> PrecedenceLevel
precedenceLevel precedence table =
  M.findWithDefault S.empty precedence (ptLevels table)

precedenceTableLevels :: PrecedenceTable -> [PrecedenceLevel]
precedenceTableLevels table = map snd (M.toAscList (ptLevels table))

addOperator :: Associativity -> Precedence -> QName -> PrecedenceTable
            -> PrecedenceTable
addOperator assoc precedence qname table =
  let level = precedenceLevel precedence table
    in PT {
         ptLevels =
           M.insert precedence (S.insert (Op qname assoc) level)
                    (ptLevels table),
         ptOperatorParts =
           ptOperatorParts table `S.union`
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

isOperatorPart :: QName -> PrecedenceTable -> Bool
isOperatorPart qname table = S.member qname (ptOperatorParts table)

