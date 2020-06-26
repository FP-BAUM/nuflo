module Desugaring.Desugaring(desugarProgram) where

import qualified Data.Set as S

import qualified Calculus.Terms as C
import Error(Error(..), ErrorType(..))
import FailState(FailState, getFS, putFS, modifyFS, evalFS, failFS, logFS)
import Position(Position(..), unknownPosition)
import Syntax.Name(QName(..))
import Syntax.GroupEquations(groupEquations)
import Syntax.AST(
         AnnProgram(..), Program,
         AnnDeclaration(..), Declaration,
         AnnSignature(..), Signature,
         AnnEquation(..), Equation,
         AnnConstraint(..), Constraint,
         AnnCaseBranch(..), CaseBranch,
         AnnExpr(..), Expr
       )

desugarProgram :: Program -> Either Error C.Term
desugarProgram program = evalFS (desugarProgramM program) initialState
  where initialState = DesugarState {
    statePosition     = unknownPosition,
    stateConstructors = S.empty
  }

data DesugarState = DesugarState {
  statePosition     :: Position,
  stateConstructors :: S.Set QName
}

declareConstructor :: QName -> M ()
declareConstructor name = do
  state <- getFS
  putFS (state { stateConstructors = S.insert name (stateConstructors state) })

type M = FailState DesugarState

currentPosition :: M Position
currentPosition = do
  state <- getFS
  return $ statePosition state

setPosition :: Position -> M ()
setPosition pos = do
  state <- getFS
  putFS (state { statePosition = pos })

failM :: ErrorType -> String -> M a
failM errorType msg = do
  pos <- currentPosition
  failFS (Error errorType pos msg)

---------------------------------

desugarProgramM :: Program -> M C.Term
desugarProgramM (Program decls) = do
  mapM_ collectConstructors decls
  equations <- case groupEquations (concatMap valueDeclaration decls) of
                 Left msg ->
                   failM DesugaringErrorDuplicatedValueDefinition msg
                 Right equations -> return equations
  terms <- mapM desugarDeclaration decls
  return $ C.Num 0
  where
    valueDeclaration :: Declaration -> [Equation]
    valueDeclaration (ValueDeclaration eq)  = [eq]
    valueDeclaration _                      = []

collectConstructors :: Declaration -> M ()
collectConstructors (DataDeclaration _ _ constructors) = do
  mapM_ (declareConstructor . signatureName) constructors

collectConstructors _ = return ()

desugarDeclaration :: Declaration -> M C.Term
desugarDeclaration _ = return $ C.Num 0
