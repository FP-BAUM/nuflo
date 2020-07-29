module Desugaring.Desugaring(desugarProgram) where

import qualified Data.Set as S
import Data.Maybe(fromJust)

import qualified Calculus.Terms as C
import Error(Error(..), ErrorType(..))
import FailState(FailState, getFS, putFS, modifyFS, evalFS, failFS, logFS)
import Position(Position(..), unknownPosition)
import Syntax.Name(QName(..), primitiveTuple, primitiveUnderscore)
import Syntax.GroupEquations(groupEquations)
import Syntax.AST(
         AnnProgram(..), Program,
         AnnDeclaration(..), Declaration,
         AnnSignature(..), Signature,
         AnnEquation(..), Equation,
         AnnConstraint(..), Constraint,
         AnnCaseBranch(..), CaseBranch,
         AnnExpr(..), Expr,
         exprFreeVariables,
         exprHeadVariable, exprHeadArguments
       )

desugarProgram :: Program -> Either Error C.Term
desugarProgram program = evalFS (desugarProgramM program) initialState
  where initialState = DesugarState {
    statePosition     = unknownPosition,
    stateConstructors = S.empty,
    stateNextFresh    = 0,
    stateEnvironment  = [S.empty]
  }

data DesugarState = DesugarState {
    statePosition     :: Position,
    stateConstructors :: S.Set QName,
    stateNextFresh    :: Integer,
    stateEnvironment  :: [S.Set QName] -- non-empty stack of ribs
  }

declareConstructor :: QName -> M ()
declareConstructor name = do
  state <- getFS
  putFS (state { stateConstructors = S.insert name (stateConstructors state) })

getConstructors :: M (S.Set QName)
getConstructors = do
  state <- getFS
  return $ stateConstructors state

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

freshVariable :: M QName
freshVariable = do
  state <- getFS
  putFS (state { stateNextFresh = stateNextFresh state + 1 })
  return $ Name ("d{" ++ show (stateNextFresh state) ++ "}")

enterScope :: M ()
enterScope = modifyFS (\ state ->
               state { stateEnvironment = S.empty : stateEnvironment state })

exitScope :: M ()
exitScope = modifyFS (\ state ->
               state { stateEnvironment = tail (stateEnvironment state) })

bindVariable :: QName -> M ()
bindVariable x = modifyFS (\ state ->
                   state {
                     stateEnvironment =
                       S.insert x (head (stateEnvironment state)) :
                       tail (stateEnvironment state)
                   })

getAllBoundVars :: M (S.Set QName)
getAllBoundVars = do
  state <- getFS
  return $ S.unions (stateEnvironment state)

getAllBoundVarsAndConstructors :: M (S.Set QName)
getAllBoundVarsAndConstructors = do
  cs <- getConstructors
  bs <- getAllBoundVars
  return (cs `S.union` bs)

---------------------------------

desugarProgramM :: Program -> M C.Term
desugarProgramM (Program decls) = do
    mapM_ collectConstructors decls
    equations <- case groupEquations (concatMap valueDeclaration decls) of
                   Left msg ->
                     failM DesugaringErrorDuplicatedValueDefinition msg
                   Right equations -> return equations
    desugarLetrec equations (EInt unknownPosition 0)

valueDeclaration :: Declaration -> [Equation]
valueDeclaration (ValueDeclaration eq)  = [eq]
valueDeclaration _                      = []

collectConstructors :: Declaration -> M ()
collectConstructors (DataDeclaration _ _ constructors) = do
  mapM_ (declareConstructor . signatureName) constructors
collectConstructors _ = return ()

desugarLetrec :: [Equation] -> Expr -> M C.Term
desugarLetrec equations body =
  let vars = map (fromJust . exprHeadVariable . equationLHS) equations
      rhss = map equationRHS equations
   in do
     rec   <- freshVariable
     enterScope
     mapM_ bindVariable vars
     rhss' <- mapM desugarExpr rhss
     body' <- desugarExpr body
     exitScope
     return $
       termLet rec (C.Fix rec
                     (termFresh vars
                       (C.Seq (C.Unif (C.Var rec)
                                      (termTuple (map C.Var vars)))
                              (termTuple rhss'))))
                   body'

termLet :: QName -> C.Term -> C.Term -> C.Term
termLet var value body = C.App (C.Lam var (progSingleton body)) value

termTuple :: [C.Term] -> C.Term
termTuple terms = foldl C.App (C.Var primitiveTuple) terms

termFresh :: [QName] -> C.Term -> C.Term
termFresh vars body = foldr C.Fresh body vars

progSingleton :: C.Term -> C.Program
progSingleton t = C.Alt t C.Fail

desugarExpr :: Expr ->  M C.Term
-- TODO: primitivas
desugarExpr (EVar _ x)
  | x == primitiveUnderscore  = do x' <- freshVariable
                                   return $ C.Fresh x' (C.Var x')
  | otherwise                 = return $ C.Var x
desugarExpr (EUnboundVar _ x) = return $ C.Var x
desugarExpr (EInt _ x)        = return $ C.Num x
desugarExpr (EApp _ e1 e2)    = do t1 <- desugarExpr e1
                                   t2 <- desugarExpr e2
                                   return $ C.App t1 t2
desugarExpr (ELambda _ e1 e2) = do
  bound <- getAllBoundVarsAndConstructors
  let freeVars = exprFreeVariables bound e1 in do
    x  <- freshVariable
    t1 <- desugarExpr e1
    enterScope
    mapM_ bindVariable freeVars
    t2 <- desugarExpr e2
    exitScope
    return $ C.Lam x (progSingleton
                       (termFresh (S.toList freeVars)
                                  (C.Seq (C.Unif (C.Var x) t1) t2)))
desugarExpr (ELet pos decls body) = do
  setPosition pos
  equations <- case groupEquations (concatMap valueDeclaration decls) of
                Left msg -> failM DesugaringErrorDuplicatedValueDefinition msg
                Right equations -> return equations
  desugarLetrec equations body
desugarExpr (ECase pos guard branches) = do
  setPosition pos
  x <- freshVariable
  guard' <- desugarExpr guard
  branches' <- mapM (desugarBranch x) branches
  let program = foldr C.Alt C.Fail branches'
  return $ C.App (C.Lam x program) guard'
  where
    desugarBranch :: QName -> CaseBranch -> M C.Term
    desugarBranch x (CaseBranch _ pattern result) = do
      boundVars <- getAllBoundVarsAndConstructors
      let freeVars = exprFreeVariables boundVars pattern
      enterScope
      mapM_ bindVariable freeVars
      pattern' <- desugarExpr pattern
      result' <- desugarExpr result
      let sequence = C.Seq (C.Unif (C.Var x) pattern') result'
      exitScope
      return $ termFresh (S.toList freeVars) sequence
desugarExpr (EFresh pos name exp) = do
  enterScope
  bindVariable name
  exp' <- desugarExpr exp
  exitScope
  return $ C.Fresh name exp'
desugarExpr (EPlaceholder _ _) =
  error "(Impossible: desugaring placeholder of an instance placeholder)"

