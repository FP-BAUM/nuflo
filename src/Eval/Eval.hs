
module Eval.Eval(eval) where

import qualified Calculus.Terms as C
import System.Exit(die)
import Eval.EvalMonad(EvalMonad, failEM, getEM, putEM, modifyEM, logEM,
                    runEM, execEM, evalEM)

data Result = End (IO ())
            | Next (IO (C.Term, Result))

data EvalState = EvalState {
    stateNextFresh :: Integer,
    stateThreads   :: [C.Term]
  }


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
  (normalForms, pending) <- splitNormalForms . concat <$> mapM stepThread threads
  putThreads pending
  return normalForms

splitNormalForms :: [C.Term] -> ([C.Term], [C.Term])
splitNormalForms []    = ([], [])
splitNormalForms (t : terms) =  let (normalForms, pending) = splitNormalForms terms
                                in if (isNormalForm t)
                                    then (t : normalForms, pending)
                                    else (normalForms, t : pending)

isNormalForm :: C.Term -> Bool
isNormalForm (C.Var _)                 = True
isNormalForm (C.Num _)                 = True
isNormalForm (C.Cons _)                = True
isNormalForm (C.Lam _ _)               = False
isNormalForm (C.LamL _ _ _)            = True
isNormalForm (C.Fix _ _)               = False
isNormalForm (C.Fresh _ _)             = False
isNormalForm (C.Seq t1 t2)             = isStuck t1 && isNormalForm t2
isNormalForm (C.Unif t1 t2)            = isNormalForm t1 && isNormalForm t2 && (notIsValue t1 || notIsValue t2)
isNormalForm (C.App (C.LamL _ _ _) t2) = isStuck t2
isNormalForm (C.App t1 t2)             = isNormalForm t1 && isNormalForm t2

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
stepThread = error "todo"