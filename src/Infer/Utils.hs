module Infer.Utils(mergeEquations) where

import Data.Maybe(fromJust)
import qualified Data.Set as S

import Error(Error(..), ErrorType(..))
import Position(Position)
import Syntax.Name(QName(..), primitiveAlternative)
import Syntax.AST(AnnEquation(..), Equation,
                  AnnCaseBranch(..), CaseBranch,
                  AnnExpr(..), Expr, exprHeadVariable, exprHeadArguments
                )

mergeEquations :: [Equation] -> Either String [Equation]
mergeEquations eqs = case duplicatedNames of
                       Right s   -> Right groupedEquations
                       Left name -> Left ("duplicated name " ++ show name)
  where
    groupedEquations = map joinEquations (groupEquations eqs)
    functionNames = map (fromJust . exprHeadVariable . equationLHS) groupedEquations
    duplicatedNames  = foldl (\set elem ->  case set of
                                            Right s   -> if S.member elem s
                                                          then Left elem
                                                          else Right $ S.insert elem s
                                            Left name -> Left name
                                ) (Right S.empty) functionNames

-- Precondition: Each equation received as param should have the same Qname
joinEquations :: [Equation] -> Equation
joinEquations []                                = error "equation group can't be empty"
joinEquations eqs@((Equation pos lhs _) : _) =
  case m of
  0 -> let rhs' = foldl1 (\e1 e2 -> EApp pos (EApp pos (EVar pos primitiveAlternative) e1) e2) (map equationRHS eqs)
       in Equation pos lhs rhs'
  _ -> let params = map parameterName [1..m]
           functionName = fromJust (exprHeadVariable lhs)
           innerBody = composeEquations params
           body = foldr (\name e -> ELambda pos (EUnboundVar pos name) e) innerBody params 
       in (Equation pos (EVar pos functionName) body)
  where
    m = maximum (map numOfArg eqs)
    composeEquations :: [QName] -> Expr
    composeEquations params = ECase pos guard (map branch eqs)
      where
        guard = makeTuple pos (map (EVar pos) params)
        
        pad :: [Expr] -> Int -> [Expr]
        pad exprs n = replicate (n - length exprs) (EVar pos (Name "_"))
        
        branch :: Equation -> CaseBranch
        branch (Equation pos lhs rhs) = let patterns = fromJust (exprHeadArguments lhs)
                                            pattern  = makeTuple pos (patterns ++ pad patterns m)
                                        in CaseBranch pos pattern rhs

makeTuple :: Position -> [Expr] -> Expr
makeTuple pos exprs = foldl (\tuple e -> EApp pos tuple e) (EVar pos (Name "{Tuple}")) exprs

parameterName ::Int -> QName
parameterName m = Name ("x{" ++ show m ++ "}")

numOfArg :: Equation -> Int
numOfArg (Equation _ lhs _) = length $ fromJust (exprHeadArguments lhs)

sameEquationHead :: QName -> Equation -> Bool
sameEquationHead name (Equation _ lhs _) = name == fromJust (exprHeadVariable lhs)

groupEquations :: [Equation] -> [[Equation]]
groupEquations [] = []
groupEquations (eq@(Equation pos lhs rhs) : xs) = let f = fromJust (exprHeadVariable lhs)
                                                      eqs = takeWhile (sameEquationHead f) xs 
                                                  in (eq : eqs) : (groupEquations $ drop (length eqs) xs)
