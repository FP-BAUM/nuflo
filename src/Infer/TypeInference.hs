module Infer.TypeInference(inferTypes) where

import qualified Data.Set as S
import qualified Data.Map as M

import FailState(FailState, getFS, putFS, modifyFS, evalFS, failFS, logFS)
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
         AnnExpr(..), Expr, exprAnnotation,
         exprIsFunctionType, exprFunctionTypeCodomain, exprEqual
       )
import Calculus.Types(ConstrainedType(..))

inferTypes :: Program -> Either Error Program
inferTypes program = evalFS (inferTypeProgramM program) initialState
  where initialState = TypeInferState {
                         statePosition       = unknownPosition,
                         stateEnvironment    = [ M.empty ]
                         -- TODO
                      }

---- Type inference monad

data TypeInferState =
     TypeInferState {
       statePosition       :: Position,
       stateEnvironment    :: [ M.Map QName ConstrainedType ] -- This never will be empty
       -- TODO
     }

type M = FailState TypeInferState


inferTypeProgramM :: Program -> M Program
inferTypeProgramM (Program decls) = do
  mapM_ collectTypeDeclarationM decls
  mapM_ collectDataDeclarationM decls
  decls' <- mapM inferTypeDeclM decls
  return $ Program decls

collectDataDeclarationM :: Declaration -> M ()
collectDataDeclarationM (DataDeclaration _ typ constructors) = do  -- data Something a b c d = [Cons | Cons a b ...]
  mapM_ collectSignatureM constructors


collectSignatureM :: Signature -> M ()
collectSignatureM (Signature _ name typ constraints) = do 
  ct <- constrainedType constraints typ
  bindType name ct


collectTypeDeclarationM :: Declaration -> M ()
-- collectTypeDeclarationM (DataDeclaration pos typ constructors) = do  -- data Something a b c d = [Cons | Cons a b ...]



-- collectTypeDeclarationM (TypeDeclaration pos typ value) = do
-- collectTypeDeclarationM (TypeSignature signature) = do
-- collectTypeDeclarationM (ClassDeclaration pos className typeName methods) = do
-- collectTypeDeclarationM (InstanceDeclaration pos className typ
--                                            constraints methods) = do
  
inferTypeDeclM :: Declaration -> M Declaration
inferTypeDeclM (DataDeclaration pos typ constructors) = do

-- inferTypeDeclM (TypeDeclaration pos typ value) = do
-- inferTypeDeclM (TypeSignature signature) = do
-- inferTypeDeclM (ClassDeclaration pos className typeName methods) = do
-- inferTypeDeclM (InstanceDeclaration pos className typ
--                                            constraints methods) = do


constrainedType :: [Constraint] -> Expr -> M ConstrainedType
constrainedType constraints expr = do
  typ <- exprToType
  let cts = map constraintToTypeConstraint constraints
  in return $ CT cts typ

bindType :: QName -> ConstrainedType -> M ()
bindType varName typ = do
  state <- getFS
  let (rib : ribs) = stateEnvironment state in
   if M.member varName rib
    then failM TypeErrorVariableAlreadyDeclared
               ("Variable \"" ++ show varName ++ "\" already declared.")
    else putFS (state { stateEnvironment = M.insert varName typ rib : ribs })