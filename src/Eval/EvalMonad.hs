module Eval.EvalMonad(EvalMonad(..)) where

  import System.IO(IO)
  import Error(Error(..), ErrorType(..))
  import FailState(FailState, getFS, putFS, modifyFS, evalFS, failFS, logFS)

  data EvalMonad a = EM (IO (FailState Error a))

  instance Functor EvalMonad where
    -- fmap :: (a -> b) -> EvalMonad a -> EvalMonad b
    fmap f (EM xio) = EM $ (fmap . fmap $ f) xio

  instance Applicative EvalMonad where
    pure a = EM (pure . pure $ a)
    EM mf <*> EM ma = EM ((<*>) <$> mf <*> ma)

  instance Monad EvalMonad where
    -- return = pure
    -- EM ma >>= f = do
    --   fs <- ma
    --   a <- fs
    --   return a