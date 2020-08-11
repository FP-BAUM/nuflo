module Desugaring.Desugaring(desugarProgram) where

import qualified Data.Set as S
import Data.Maybe(fromJust)

import qualified Calculus.Terms as C
import Error(Error(..), ErrorType(..))
import FailState(FailState, getFS, putFS, modifyFS, evalFS, failFS, logFS)
import Position(Position(..), unknownPosition)
import Syntax.Name(
         QName(..),
         primitiveAlternative, primitiveUnit, primitiveTuple,
         primitivePrint, primitiveUnderscore
       )
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
    stateConstructors = S.fromList builtinConstructors,
    stateNextFresh    = 0,
    stateEnvironment  = [S.empty]
  }

builtinConstructors :: [QName]
builtinConstructors = [primitiveAlternative, primitiveTuple]

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

isBoundToConstructor :: QName -> M Bool
isBoundToConstructor x = do
  cs <- getConstructors
  bs <- getAllBoundVars
  return (S.member x cs && not (S.member x bs))

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

     {- DEBUG del evaluador sin letrec -}
     {-
     return $
       foldr
          (\ (x, e) b -> termLet x e b)
          body'
          (zip vars rhss')
     -}

     -- Traducción del letrec à la Leroy
     return $ termLetrec (zip vars rhss') body'

     {- Traducción original del letrec problemática porque
        no anda en call-by-value -}
     {-
     return $
       termLet rec (C.Fix rec
                     (termFresh vars
                       (C.Seq (C.Unif (C.Var rec)
                                      (termTuple (map C.Var vars)))
                              (termTuple rhss'))))
                   body'
     -}

-- letrec x1 = e1
--        ...
--        xn = en
--     in b
--
-- is desugared as
--
-- let x1 = fix(x1. \x2...xn. e1)
--     x2 = fix(x2. \x3...xn.
--                let x1 = x1 x2 ... xn     in
--                  e2)
--     ...
--     xk = fix(xk. \xk+1...xn.
--                let xk-1 = xk-1 xk ... xn in
--                ...
--                let x2   = x2 ... xn      in
--                let x1   = x1 x2 ... xn   in
--                  ek)
--     ...
--     xn = fix(xn. let xn-1 = xn-1 xn    in
--                  ...
--                  let x1 = x1 x2 ... xn in
--                    en)
-- in let xn-1 = xn-1 xn in
--    ... 
--    let x2 = x2 x3 ... xn in
--    let x1 = x1 x2 ... xn in
--      b
termLetrec :: [(QName, C.Term)] -> C.Term -> C.Term
termLetrec bindings body = rec [] bindings body
  where
    rec :: [QName] -> [(QName, C.Term)] -> C.Term -> C.Term
    rec prev []                    body = bindPrev prev [] body
    rec prev ((var, value) : next) body =
      let next' = map fst next in
        termLet var
                (C.Fix var
                       (bindNext next'
                         (bindPrev prev next' value)))
                (rec (var : prev) next body)

    -- bindNext [xk+1, ..., xn] e
    -- builds the term
    --   "\ xk+1 ... xn -> e"
    bindNext :: [QName] -> C.Term -> C.Term
    bindNext next e = foldr C.lam e next

    -- bindPrev [xk, ..., x1] [xk+1, ..., xn] e
    -- builds the term
    --   "let xk = xk xk+1 ... xn
    --        ...
    --        x1 = x1 x2 ... xn
    --     in e"
    bindPrev :: [QName] -> [QName] -> C.Term -> C.Term
    bindPrev []           next e = e
    bindPrev (var : prev) next e =
      termLet var (app var next)
              (bindPrev prev (var : next) e)

    -- app xk [xk+1, ..., xn]
    -- builds the term "xk xk+1 ... xn"
    app :: QName -> [QName] -> C.Term
    app head args = foldl C.App (C.Var head) (map C.Var args)

termLet :: QName -> C.Term -> C.Term -> C.Term
termLet var value body = C.App (C.Lam var (progSingleton body)) value

termTuple :: [C.Term] -> C.Term
termTuple terms = foldl C.App (C.Cons primitiveTuple) terms

termFresh :: [QName] -> C.Term -> C.Term
termFresh vars body = foldr C.Fresh body vars

progSingleton :: C.Term -> C.Program
progSingleton t = C.Alt t C.Fail

desugarExpr :: Expr ->  M C.Term
---- Begin: primitives
-- Nullary
desugarExpr (EVar _ x)
  | x == primitiveUnit        = return C.consOk
-- Unary
desugarExpr (EApp _ (EVar _ x) e)
  | x == primitivePrint       =
    do t <- desugarExpr e
       return $ C.Primitive C.Print [t]
-- Binary
desugarExpr (EApp _ (EApp _ (EVar _ x) e1) e2)
  | x == primitiveAlternative =
    do t1 <- desugarExpr e1
       t2 <- desugarExpr e2
       z <- freshVariable
       return $ C.App (C.Lam z (C.Alt t1 (C.Alt t2 C.Fail))) C.consOk
---- End: primitives
desugarExpr (EVar _ x)
  | x == primitiveUnderscore  = do x' <- freshVariable
                                   return $ C.Fresh x' (C.Var x')
  | otherwise                 = do
      b <- isBoundToConstructor x
      return $ if b
                then C.Cons x
                else C.Var x
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

