module Eval.Eval(evalProgram) where

import qualified Calculus.Terms as C
import Eval.EvalMonad(EvalMonad, failEM, getEM, putEM, modifyEM, logEM,
                    runEM, execEM, evalEM)

data Result = End (IO ())
            | Next (IO (C.Term, Result))

evalProgram = error "TODO"

-- evalTerm :: C.Term -> Result
-- evalTerm term = error

-- contract :: Term -> [QName] -> ([Goal], [Term])
-- contract v@(Var _)         = ([], [v])
-- contract c@(Cons _)        = ([], [c])
-- contract (Fresh name term) =
--   let (goals, prog) = contract term
--   in ()