
module Infer.KindInference(inferKinds) where

import FailState(FailState, getFS, modifyFS, evalFS, failFS)
import Error(Error(..), ErrorType(..))
import Position(Position(..), unknownPosition)
import Syntax.Name(QName)
import Syntax.AST(
         AnnProgram(..), Program,
         AnnDeclaration(..), Declaration,
         AnnSignature(..), Signature,
         AnnEquation(..), Equation,
         AnnConstraint(..), Constraint,
         AnnCaseBranch(..), CaseBranch,
         AnnExpr(..), Expr, exprAnnotation
       )

inferKinds :: Program -> Either Error ()
inferKinds program = evalFS (ikindProgramM program) initialState
  where initialState = KindInferState {
                        statePosition = unknownPosition
                       }

---- Kind inference monad

data KindInferState = KindInferState {
                        statePosition :: Position
                      }

type M = FailState KindInferState

failM :: ErrorType -> String -> M a
failM errorType msg = do
  pos <- currentPosition
  failFS (Error errorType pos msg)

setPosition :: Position -> M ()
setPosition pos = modifyFS (\ state -> state { statePosition = pos })

currentPosition :: M Position
currentPosition = do
  state <- getFS
  return $ statePosition state

---- Kind inference algorithm

ikindProgramM :: Program -> M ()
ikindProgramM (Program decls) = do
  mapM_ declareDatatypeM decls
  mapM_ ikindDeclarationM decls

-- Split the left-hand side in the definition of a datatype,
-- such as "Map a b" into the head "Map" and the arguments ["a", "b"]
-- checking that it is well-formed.
--
-- All the arguments should be variables.
splitDatatypeArgs :: Expr -> M (QName, [QName])
splitDatatypeArgs (EApp _ f (EVar _ x)) = do
  (head, args) <- splitDatatypeArgs f
  return (head, args ++ [x])
splitDatatypeArgs (EVar _ x) =
  return (x, [])
splitDatatypeArgs expr = do
  setPosition (exprAnnotation expr)
  failM KindErrorMalformedDatatype "Malformed datatype."

declareDatatypeM :: Declaration -> M ()
declareDatatypeM (DataDeclaration pos name constructors) = do
  splitDatatypeArgs name
  --TODO
  return ()
declareDatatypeM _ = return ()

ikindDeclarationM :: Declaration -> M ()
ikindDeclarationM (DataDeclaration pos name constructors) =
  -- TODO: check constructors
  return ()
ikindDeclarationM (TypeDeclaration pos name value) =
  error "NOT IMPLEMENTED"
ikindDeclarationM (TypeSignature signature) =
  error "NOT IMPLEMENTED"
ikindDeclarationM (ValueDeclaration equation) =
  error "NOT IMPLEMENTED"
ikindDeclarationM (ClassDeclaration pos name typ methods) =
  error "NOT IMPLEMENTED"
ikindDeclarationM (InstanceDeclaration pos name typ constratints methods) =
  error "NOT IMPLEMENTED"

