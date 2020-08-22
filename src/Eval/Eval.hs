
module Eval.Eval(eval, evalInNamespace) where

import System.Exit(die)
import qualified Data.Map as M

import qualified Calculus.Terms as C
import Syntax.Name(QName(..))
import ModuleSystem.Namespace(Namespace, emptyNamespace)
import Eval.EvalMonad(EvalMonad, failEM, getEM, putEM, modifyEM, logEM,
                      putStrLnEM,
                      runEM, execEM, evalEM)
import Calculus.Pprint(pprintInNamespace)

data Result = End (IO ())
            | Next (IO (C.Term, Result))

data EvalState = EvalState {
                   stateNextFresh    :: Integer
                 , stateNextLocation :: Integer
                 , stateThreads      :: [C.Term]
                 }

data Goal = Goal C.Term C.Term
  deriving Show

type Subst = M.Map QName C.Term

freshVar :: M QName
freshVar = do
  state <- getEM
  putEM (state { stateNextFresh = stateNextFresh state + 1 })
  return $ Name ("?{" ++ show (stateNextFresh state) ++ "}")

freshLocation :: M Integer
freshLocation = do
  state <- getEM
  putEM (state { stateNextLocation = stateNextLocation state + 1 })
  return $ stateNextLocation state

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
eval term = evalInNamespace emptyNamespace term

evalInNamespace :: Namespace -> C.Term -> IO ()
evalInNamespace namespace term = rec initialState
  where
    initialState :: EvalState
    initialState = EvalState {
                     stateNextFresh    = 0
                   , stateNextLocation = 0
                   , stateThreads      = [term]
                   }
    rec :: EvalState -> IO ()
    rec state = do
      result <- runEM step state
      case result of
        Left err -> do
          die ("Runtime error:\n" ++ show err)
        Right (normalForms, state') -> do
          {- Show final result -}
          mapM_ (runNormalForm namespace) normalForms
          {--}
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
   in if isNormalForm t
       then (t : normalForms, pending)
       else (normalForms, t : pending)

isNormalForm :: C.Term -> Bool
isNormalForm (C.Var _)         = True
isNormalForm (C.Num _)         = True
isNormalForm (C.Cons _)        = True
isNormalForm (C.Lam _ _)       = False
isNormalForm (C.LamL _ _ _)    = True
isNormalForm (C.Fix _ _)       = False
isNormalForm (C.Fresh _ _)     = False
isNormalForm (C.Seq t1 t2)     = isStuck t1 && isNormalForm t2
isNormalForm (C.Unif t1 t2)    = isNormalForm t1 && isNormalForm t2 &&
                                 (notIsValue t1 || notIsValue t2)
isNormalForm (C.Function _ ts) = all isNormalForm ts && any isStuck ts
isNormalForm (C.Command  _ ts) = all isNormalForm ts
isNormalForm
  (C.App (C.LamL _ _ _) t2)    = isStuck t2
isNormalForm (C.App t1 t2)     = isNormalForm t1 && isNormalForm t2

isVar :: C.Term -> Bool
isVar (C.Var _)      = True
isVar _              = False

programToList :: C.Program -> [C.Term]
programToList C.Fail      = []
programToList (C.Alt t p) = t : programToList p

isFree :: QName -> C.Term -> Bool
isFree x (C.Var y)          = x == y
isFree x (C.Cons _)         = False
isFree x (C.Num _)          = False
isFree x (C.Fresh y t)      = x /= y && isFree x t
isFree x (C.Lam y p)        = x /= y && isFreeP x p
isFree x (C.LamL _ y p)     = x /= y && isFreeP x p
isFree x (C.Fix y t)        = x /= y && isFree x t
isFree x (C.App t1 t2)      = isFree x t1 || isFree x t2
isFree x (C.Seq t1 t2)      = isFree x t1 || isFree x t2
isFree x (C.Unif t1 t2)     = isFree x t1 || isFree x t2
isFree x (C.Function _ ts)  = any (isFree x) ts
isFree x (C.Command _ ts)   = any (isFree x) ts

isFreeP :: QName -> C.Program -> Bool
isFreeP x C.Fail      = False
isFreeP x (C.Alt t p) = isFree x t || isFreeP x p

isValue :: C.Term -> Bool
isValue (C.Num _)        = True
isValue (C.Var _)        = True
isValue (C.LamL _ _ _)   = True
isValue (C.Command _ ts) = all isValue ts
isValue t                = isStructure t

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
  mSubst <- mgu goals
  case mSubst of
    Nothing    -> return []
    Just subst -> mapM (applySubst subst) terms

stepThread' :: C.Term -> M ([Goal], [C.Term])
stepThread' e@(C.Var _)      = return ([], [e])
stepThread' e@(C.Cons _)     = return ([], [e])
stepThread' e@(C.Num _)      = return ([], [e])
stepThread' (C.Fresh x t)    = do x' <- freshVar
                                  t' <- renameVar t x x'
                                  stepThread' t'
stepThread' (C.Lam x p)      = do l <- freshLocation
                                  return ([], [C.LamL l x p])
stepThread' e@(C.LamL _ _ _) = do l <- freshLocation
                                  return ([], [e])
stepThread' (C.App (C.LamL _ x p) v)
  | isValue v                = do
    p' <- applySubstP (singletonSubst x v) p
    return ([], programToList p')
stepThread' e@(C.Fix x t)    = do
  t' <- applySubst (singletonSubst x e) t
  return ([], [t'])
stepThread' (C.App t1 t2)    = do (g1, p1) <- stepThread' t1
                                  (g2, p2) <- stepThread' t2
                                  return (g1 ++ g2, C.App <$> p1 <*> p2)
stepThread' (C.Seq v t)
  | isValue v                = stepThread' t
stepThread' (C.Seq t1 t2)    = do (g1, p1) <- stepThread' t1
                                  (g2, p2) <- stepThread' t2
                                  return (g1 ++ g2, C.Seq <$> p1 <*> p2)
stepThread' (C.Unif v w)
  | isValue v && isValue w   = return ([Goal v w], [C.consOk])
---- (Experimental swap rules)
--stepThread' (C.Unif t1 (C.Seq t2 t3)) = return ([], [C.Seq t2 (C.Unif t1 t3)])
--stepThread' (C.Unif (C.Seq t1 t2) t3) = return ([], [C.Seq t1 (C.Unif t2 t3)])
---- (End of experimental swap rules)
stepThread' (C.Unif t1 t2)   = do (g1, p1) <- stepThread' t1
                                  (g2, p2) <- stepThread' t2
                                  return (g1 ++ g2, C.Unif <$> p1 <*> p2)
stepThread' (C.Function f ts)
  | all isValue ts            = do p <- applyPrimitiveFunction f ts
                                   return ([], p)
stepThread' (C.Function f ts) = do
  (gs, ps) <- unzip <$> mapM stepThread' ts
  return (concat gs, [C.Function f ts' | ts' <- sequence ps])
stepThread' (C.Command f ts) = do
  (gs, ps) <- unzip <$> mapM stepThread' ts
  return (concat gs, [C.Command f ts' | ts' <- sequence ps])

-- Note: all the arguments are already values
applyPrimitiveFunction :: C.PrimitiveFunction -> [C.Term] -> M [C.Term]
applyPrimitiveFunction C.IntAdd [C.Num x, C.Num y] = do
  return [C.Num (x + y)]
--applyPrimitiveFunction C.Print [v] = do
--  putStrLnEM (show v)
--  return [C.consOk]
applyPrimitiveFunction f args =
  error ("Unimplemented primitive function " ++ show f
         ++ " with arity " ++ show (length args))

runNormalForm :: Namespace -> C.Term -> IO ()
runNormalForm namespace (C.Command C.Print [t]) = do
  putStrLn (pprintInNamespace namespace t)
runNormalForm _ (C.Command f args) =
  error ("Unimplemented primitive command " ++ show f
         ++ " with arity " ++ show (length args))
runNormalForm _ _ = return ()

singletonSubst :: QName -> C.Term -> Subst
singletonSubst x v = M.insert x v M.empty

mgu :: [Goal] -> M (Maybe Subst)
mgu []         = return (Just M.empty)
mgu (Goal (C.Num n) (C.Num m) : xs)
  | n == m     = mgu xs
mgu (Goal (C.Var x) (C.Var y) : xs)
  | x == y     = mgu xs
mgu (Goal (C.Var x) v : xs)
  | isFree x v = return Nothing
mgu (Goal (C.Var x) v : xs) = do
  xs' <- mapM (applySubstG (singletonSubst x v)) xs
  mSubst <- mgu xs'
  case mSubst of
    Nothing -> return Nothing
    Just subst -> do
      v' <- applySubst subst v
      return $ Just (M.insert x v' subst)
mgu (Goal v (C.Var x) : xs)
  | not $ isVar v   = mgu (Goal (C.Var x) v : xs)
mgu (Goal (C.LamL l _ _) (C.LamL l' _ _) : xs)
  | l == l'         = mgu xs
mgu (Goal (C.LamL l _ _) (C.LamL l' _ _) : xs)
                    = return Nothing
mgu (Goal v w : xs) =
  let (v', v_args) = C.splitArgs v
      (w', w_args) = C.splitArgs w
   in case (v', w') of
     (C.Cons c, C.Cons d)
       | c == d && length v_args == length w_args
       -> mgu (zipWith Goal v_args w_args ++ xs)
     _ -> return Nothing

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
applySubst subst (C.Function f ts) = do
  ts' <- mapM (applySubst subst) ts
  return $ C.Function f ts'
applySubst subst (C.Command f ts) = do
  ts' <- mapM (applySubst subst) ts
  return $ C.Command f ts'

applySubstP :: Subst -> C.Program -> M C.Program
applySubstP subst C.Fail      = return C.Fail
applySubstP subst (C.Alt t p) = do
  t' <- applySubst subst t
  p' <- applySubstP subst p
  return $ C.Alt t' p'

applySubstG :: Subst -> Goal -> M Goal
applySubstG subst (Goal t1 t2) = do
  t1' <- applySubst subst t1
  t2' <- applySubst subst t2
  return $ Goal t1' t2'

renameVar :: C.Term -> QName -> QName -> M C.Term
renameVar t x y = applySubst (singletonSubst x (C.Var y)) t

