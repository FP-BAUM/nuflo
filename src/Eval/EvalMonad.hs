module Eval.EvalMonad(
         EvalMonad, failEM, getEM, putEM, modifyEM, logEM,
         putStrLnEM,
         runEM, execEM, evalEM
       ) where

import Debug.Trace(trace)

import System.IO(IO)
import Error(Error(..), ErrorType(..))
import FailState(FailState, getFS, putFS, modifyFS, evalFS, failFS, logFS)

data EvalMonad state a = EM (state -> IO (Either Error (a, state)))

instance Functor (EvalMonad state) where
  fmap f (EM a) = EM $ \ s0 -> do
    a' <- a s0
    return $ case a' of
      Left err        -> Left err
      Right (a'', s1) -> Right (f a'', s1)

instance Applicative (EvalMonad state) where
  pure a        = EM $ \ s0 -> return $ Right (a, s0)
  EM f <*> EM a = EM $ \ s0 -> do
    f' <- f s0
    case f' of
      Left err        -> return $ Left err
      Right (f'', s1) -> do
        a' <- a s1
        return $ case a' of
          Left err        -> Left err
          Right (a'', s2) -> Right (f'' a'', s2)

instance Monad (EvalMonad state) where
  return = pure
  EM a >>= f = EM $ \ s0 -> do
    a' <- a s0
    case a' of
      Left  err       -> return $ Left err
      Right (a'', s1) -> let EM b = f a'' in b s1

failEM :: Error -> EvalMonad state a
failEM e = EM $ \ s0 -> return (Left e)

getEM :: EvalMonad state state
getEM = EM $ \ s0 -> return (Right (s0, s0))

putEM :: state -> EvalMonad state ()
putEM s = EM $ \ _ -> return (Right ((), s))

modifyEM :: (state -> state) -> EvalMonad state ()
modifyEM f = EM $ \ s0 -> return (Right ((), f s0))

logEM :: String -> EvalMonad state ()
logEM msg = trace msg (return ())

liftIO_EM :: IO a -> EvalMonad state a
liftIO_EM cmd = EM $ \ s -> do
                  a <- cmd
                  return (Right (a, s))

putStrEM :: String -> EvalMonad state ()
putStrEM str = liftIO_EM (putStr str)

putStrLnEM :: String -> EvalMonad state ()
putStrLnEM str = liftIO_EM (putStrLn str)

runEM :: EvalMonad state a -> state -> IO (Either Error (a, state))
runEM (EM a) s0 = a s0

evalEM :: EvalMonad state a -> state -> IO (Either Error a)
evalEM a s0 = do
  a' <- runEM a s0
  case a' of
    Left err       -> return $ Left err
    Right (a'', _) -> return $ Right a''

execEM :: EvalMonad state a -> state -> IO (Either Error state)
execEM a s0 = do
  a' <- runEM a s0
  case a' of
    Left err      -> return $ Left err
    Right (_, s1) -> return $ Right s1

