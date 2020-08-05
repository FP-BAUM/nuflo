
module Eval.Eval(eval) where

import System.Exit(die)

import qualified Calculus.Terms as C
import qualified Data.Map as M
import Syntax.Name(QName(..))
import Eval.EvalMonad(EvalMonad, failEM, getEM, putEM, modifyEM, logEM,
                      runEM, execEM, evalEM)

data Result = End (IO ())
            | Next (IO (C.Term, Result))

data EvalState = EvalState {
                   stateNextFresh :: Integer
                 , stateThreads   :: [C.Term]
                 }

data Goal = Goal C.Term C.Term

type Subst = M.Map QName C.Term

freshVar :: M QName
freshVar = do
  state <- getEM
  let var = stateNextFresh state
  putEM (state { stateNextFresh = (stateNextFresh state) + 1 })
  return $ Name ("?{" ++ show var ++ "}")

getThreads :: M [C.Term]
getThreads = do
  state <- getEM
  return $ stateThreads state

putThreads :: [C.Term] -> M ()
putThreads threads = do
  state <- getEM
  putEM (state { stateThreads = threads })

type M = EvalMonad EvalState

eval :: C.Term -> IO ()
eval term = rec initialState
  where
    initialState :: EvalState
    initialState = EvalState { stateNextFresh = 0, stateThreads = [term] }
    rec :: EvalState -> IO ()
    rec state = do
      result <- runEM step state
      case result of
        Left err -> do
          die ("Runtime error:\n" ++ show err)
        Right (normalForms, state') -> do
          mapM_ (putStrLn . show) normalForms
          if null $ stateThreads state'
           then return ()
           else rec state'

step :: M [C.Term]
step = do
  threads <- getThreads
  (normalForms, pending) <- splitNormalForms . concat <$>
                            mapM stepThread threads
  putThreads pending
  return normalForms

splitNormalForms :: [C.Term] -> ([C.Term], [C.Term])
splitNormalForms []          = ([], [])
splitNormalForms (t : terms) =
  let (normalForms, pending) = splitNormalForms terms
   in if (isNormalForm t)
       then (t : normalForms, pending)
       else (normalForms, t : pending)

isNormalForm :: C.Term -> Bool
isNormalForm (C.Var _)      = True
isNormalForm (C.Num _)      = True
isNormalForm (C.Cons _)     = True
isNormalForm (C.Lam _ _)    = False
isNormalForm (C.LamL _ _ _) = True
isNormalForm (C.Fix _ _)    = False
isNormalForm (C.Fresh _ _)  = False
isNormalForm (C.Seq t1 t2)  = isStuck t1 && isNormalForm t2
isNormalForm (C.Unif t1 t2) = isNormalForm t1 && isNormalForm t2 &&
                              (notIsValue t1 || notIsValue t2)
isNormalForm
  (C.App (C.LamL _ _ _) t2) = isStuck t2
isNormalForm (C.App t1 t2)  = isNormalForm t1 && isNormalForm t2

isVar :: C.Term -> Bool
isVar (C.Var _)      = True
isVar _              = False

isFree :: QName -> C.Term -> Bool
isFree = error "not implemented"
-- isFree



-- data Term = Var QName
--           | Cons QName
--           | Num Integer
--           | Fresh QName Term
--           | Lam QName Program
--           | LamL Location QName Program
--           | Fix QName Term
--           | App Term Term
--           | C.Seq Term Term
--           | C.Unif Term Term

isValue :: C.Term -> Bool
isValue (C.Var _)      = True
isValue (C.LamL _ _ _) = True
isValue t              = isStructure t

notIsValue :: C.Term -> Bool
notIsValue = not . isValue

isStructure :: C.Term -> Bool
isStructure (C.App s t) = isStructure s && isValue t
isStructure (C.Cons _)  = True
isStructure _           = False

isStuck :: C.Term -> Bool
isStuck t = isNormalForm t && not (isValue t)

stepThread :: C.Term -> M [C.Term]
stepThread term = do
  (goals, terms) <- stepThread' term
  subst <- mgu goals
  case subst of
    Nothing     -> return []
    Just subst' -> mapM (applySubst subst') terms

stepThread' :: C.Term -> M ([Goal], [C.Term])
stepThread' var@(C.Var _)                = return ([], [var])
stepThread' const@(C.Cons _)             = return ([], [const])
stepThread' num@(C.Num _)                = return ([], [num])
stepThread' fresh@(C.Fresh x term)       = do
                                          x' <- freshVar
                                          term' <- renameVar term x x'
                                          stepThread' term'
stepThread' _ = error "bla"
-- stepThread' lambda@(Lam name programm) = ]

mgu :: [Goal] -> M (Maybe Subst)
mgu []                                                    = return (Just M.empty)
mgu (Goal (C.Var x) (C.Var y) :xs)        | x == y        = mgu xs
mgu (Goal (C.Var x) v :xs)                | isFree x v    = return Nothing
mgu (Goal (C.Var x) v :xs)                                = do
  xs' <- mapM (applySubstG (M.insert x v M.empty)) xs
  mSubst <- mgu xs'
  case mSubst of
    Nothing -> return Nothing
    Just subst -> do
      v' <- applySubst subst v
      return $ Just (M.insert x v' subst)
mgu (Goal v (C.Var x) :xs)                | not $ isVar v    = mgu (Goal (C.Var x) v : xs)
mgu (Goal (C.LamL l _ _) (C.LamL l' _ _) :xs) | l == l'       = mgu xs
mgu (Goal (C.LamL l _ _) (C.LamL l' _ _) :xs)                 = return Nothing
mgu (Goal v w : xs)                                           =
  let (v', v_args) = splitArgs v
      (w', w_args) = splitArgs w
  in case (v', w') of
    (C.Cons c, C.Cons d) | c == d && length v_args == length w_args -> mgu (zipWith Goal v_args w_args ++ xs)
    _                                                               -> return Nothing

applySubst :: Subst -> C.Term -> M C.Term
applySubst subst var@(C.Var name) = do
  return $ M.findWithDefault var name subst
applySubst subst const@(C.Cons _) = return const
applySubst subst num@(C.Num _)    = return num
applySubst subst (C.Fresh x term) = do
  x' <- freshVar
  term' <- applySubst (M.insert x (C.Var x') subst) term
  return $ C.Fresh x' term'
applySubst subst (C.Lam x p) = do
  x' <- freshVar
  p' <- applySubstP (M.insert x (C.Var x') subst) p
  return $ C.Lam x' p'
applySubst subst (C.LamL l x p) = do
  x' <- freshVar
  p' <- applySubstP (M.insert x (C.Var x') subst) p
  return $ C.LamL l x' p'
applySubst subst (C.Fix x t) = do
  x' <- freshVar
  t' <- applySubst (M.insert x (C.Var x') subst) t
  return $ C.Fix x' t'
applySubst subst (C.App t1 t2) = do
  t1' <- applySubst subst t1
  t2' <- applySubst subst t2
  return $ C.App t1' t2'
applySubst subst (C.Seq t1 t2) = do
  t1' <- applySubst subst t1
  t2' <- applySubst subst t2
  return $ C.Seq t1' t2'
applySubst subst (C.Unif t1 t2) = do
  t1' <- applySubst subst t1
  t2' <- applySubst subst t2
  return $ C.Unif t1' t2'

applySubstP :: Subst -> C.Program -> M C.Program
applySubstP subst C.Fail      = return C.Fail
applySubstP subst (C.Alt t p) = do
  t' <- applySubst subst t
  p' <- applySubstP subst p
  return $ C.Alt t' p'

renameVar :: C.Term -> QName -> QName -> M C.Term
renameVar t x y = applySubst (M.insert x (C.Var y) M.empty) t
