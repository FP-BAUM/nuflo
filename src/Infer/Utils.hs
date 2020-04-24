module Infer.Utils() where

import Error(Error(..), ErrorType(..))

import Syntax.Name(QName(..))
import Data.Maybe(fromJust)
import Syntax.AST(AnnEquation(..), Equation,
                  Expr, exprHeadVariable, exprHeadArguments
                )

f :: [Equation] -> Either Error [Equation]
f = return . map joinEquations . groupEquations

joinEquations :: [Equation] -> Equation
joinEquations eqs@((Equation pos lhs rhs) : xs) =
  case maximum (map numOfArg eqs) of
  0 -> error "not implemented"
  m -> let params = map (parameterName pos) [1..m]
           head = fromJust (exprHeadVariable lhs)
           body = foldr (\name e -> ELambda pos (EUnboundVar pos name) e) innerBody params 
       in (Equation pos (EVar pos head) body)

parameterName ::Integer -> QName
parameterName m = Name "x{" ++ show m ++ "}"

numOfArg :: Equation -> Integer
numOfArg (Equation _ lhs _) = length $ fromJust (exprHeadArguments lhs)

sameEquationHead :: QName -> Equation -> Bool
sameEquationHead name (Equation _ lhs _) = name == fromJust (exprHeadVariable lhs)

groupEquations :: [Equation] -> [[Equation]]
groupEquations [] = []
groupEquations (eq@(Equation pos lhs rhs) : xs) = let f = fromJust (exprHeadVariable lhs)
                                                      eqs = takeWhile (sameEquationHead f) xs 
                                                  in (eq : eqs) : (groupEquations $ drop (length eqs) xs)
