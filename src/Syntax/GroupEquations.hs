module Syntax.GroupEquations(groupEquations) where

import Data.Maybe(fromJust)
import qualified Data.Set as S

import Error(Error(..), ErrorType(..))
import Position(Position)
import Syntax.Name(QName(..), primitiveAlternative, primitiveTuple,
                   primitiveUnderscore)
import Syntax.AST(AnnEquation(..), Equation,
                  AnnCaseBranch(..), CaseBranch,
                  AnnExpr(..), Expr, exprHeadVariable, exprHeadArguments,
                  exprAlternative
                )

groupEquations :: [Equation] -> Either String [Equation]
groupEquations eqs =
  case duplicatedNames of
    Right s   -> Right groupedEqs
    Left name -> Left ("Duplicated declaration for \"" ++ show name ++ "\".")
  where
    groupedEqs    = map joinEquations (groupEquationsByName eqs)
    functionNames = map (fromJust . exprHeadVariable . equationLHS)
                        groupedEqs
    duplicatedNames =
      foldl (\ set elem ->
              case set of
                Right s -> if S.member elem s
                            then Left elem
                            else Right $ S.insert elem s
                Left name -> Left name)
            (Right S.empty)
            functionNames

-- Precondition: all the equations define the same function name
joinEquations :: [Equation] -> Equation
joinEquations [] = error "(Group of equations cannot be empty)"
joinEquations eqs@(Equation pos0 lhs0 _ : _) =
  let functionName = fromJust (exprHeadVariable lhs0) in
    Equation pos0 (EVar pos0 functionName)
             (foldr (\ name -> ELambda pos0 (EUnboundVar pos0 name))
                   (foldl1 exprAlternative (map applyEquation eqs))
                   paramNames)
  where
    maxArgs = maximum (map numberOfArguments eqs)
    paramNames = map mangleParameterName [1..maxArgs]
    params     = map (EVar pos0) paramNames
    applyEquation (Equation pos lhs rhs) =
      let patterns = fromJust (exprHeadArguments lhs) in
        foldl (EApp pos)
              (foldr (ELambda pos) rhs patterns)
              params

mangleParameterName :: Int -> QName
mangleParameterName i = Name ("param{" ++ show i ++ "}")

makeTuple :: Position -> [Expr] -> Expr
makeTuple pos exprs = foldl (EApp pos) (EVar pos primitiveTuple) exprs

numberOfArguments :: Equation -> Int
numberOfArguments (Equation _ lhs _) =
  length $ fromJust (exprHeadArguments lhs)

sameEquationHead :: QName -> Equation -> Bool
sameEquationHead name (Equation _ lhs _) =
  name == fromJust (exprHeadVariable lhs)

groupEquationsByName :: [Equation] -> [[Equation]]
groupEquationsByName [] = []
groupEquationsByName (eq@(Equation _ lhs _) : xs) =
  let functionName = fromJust (exprHeadVariable lhs)
      eqs = takeWhile (sameEquationHead functionName) xs 
   in (eq : eqs) : (groupEquationsByName $ drop (length eqs) xs)

