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
         AnnExpr(..), Expr
       )
import Calculus.Types(
         TypeMetavariable, TypeConstraint(..),
         TypeScheme(..),  ConstrainedType(..), Type(..)
       )

inferTypes :: Program -> Either Error Program
inferTypes program = evalFS (inferTypeProgramM program) initialState
  where initialState = TypeInferState {
                         statePosition    = unknownPosition,
                         stateEnvironment = [M.empty]
                         -- TODO
                      }

---- Type inference monad

data TypeInferState =
     TypeInferState {
       statePosition    :: Position,
       stateEnvironment :: [M.Map QName TypeScheme] -- Non-empty stack of ribs
       -- TODO
     }

type M = FailState TypeInferState

failM :: ErrorType -> String -> M a
failM errorType msg = do
  pos <- currentPosition
  failFS (Error errorType pos msg)

setPosition :: Position -> M ()
setPosition pos = modifyFS (\ state -> state { statePosition = pos })

currentPosition :: M Position
currentPosition = do
  state <- getFS
  return $ statePosition state

bindType :: QName -> TypeScheme -> M ()
bindType varName typ = do
  state <- getFS
  let (rib : ribs) = stateEnvironment state in
   if M.member varName rib
    then failM TypeErrorVariableAlreadyDeclared
               ("Variable \"" ++ show varName ++ "\" already declared.")
    else putFS (state { stateEnvironment = M.insert varName typ rib : ribs })


enterScopeM :: M ()
enterScopeM = modifyFS (\ state -> state {
                stateEnvironment = M.empty : stateEnvironment state
              })

exitScopeM :: M ()
exitScopeM = modifyFS (\ state -> state {
               stateEnvironment = tail (stateEnvironment state)
             })

---- Type inference algorithm

inferTypeProgramM :: Program -> M Program
inferTypeProgramM (Program decls) = do
  mapM_ collectTypeDeclarationM decls
  mapM_ collectSignaturesM decls
  decls' <- mapM inferTypeDeclM decls
  return $ Program decls

collectTypeDeclarationM :: Declaration -> M ()
collectTypeDeclarationM (TypeDeclaration pos typ value) =
  error "NOT IMPLEMENTED"
collectTypeDeclarationM _ = return ()

collectSignaturesM :: Declaration -> M ()
collectSignaturesM (DataDeclaration pos typ constructors) = do
  mapM_ collectSignatureM constructors
collectSignaturesM (TypeSignature signature) = collectSignatureM signature
collectSignaturesM _ = return ()

inferTypeDeclM :: Declaration -> M Declaration
inferTypeDeclM decl@(DataDeclaration _ _ _) =
  -- TODO: transform constraints in constructor signatures
  return decl
inferTypeDeclM (TypeDeclaration pos typ value) =
  error "NOT IMPLEMENTED"
inferTypeDeclM (ValueDeclaration equation) = do
  eq <- inferEquationM equation
  return $ ValueDeclaration eq
  
inferTypeDeclM signature@(TypeSignature _) = 
  -- TODO: transform constraints in signatures
  return signature
inferTypeDeclM (ClassDeclaration pos className typeName methods) =
  error "NOT IMPLEMENTED"
inferTypeDeclM (InstanceDeclaration pos className typ
                                        constraints methods) =
  error "NOT IMPLEMENTED"

collectSignatureM :: Signature -> M ()
collectSignatureM (Signature pos name typ constraints) = do 
  setPosition pos
  ct <- constrainedType constraints typ
  bindType name (TypeScheme [] ct) -- TODO: generalize variables

constrainedType :: [Constraint] -> Expr -> M ConstrainedType
constrainedType constraints expr =
    return $ ConstrainedType cts typ
  where
    typ = exprToType expr
    cts = map constraintToTypeConstraint constraints

inferEquationM :: Equation -> M Equation
inferEquationM (Equation pos lhs rhs) = do
  -- TODO: transform constraints
  enterScopeM
  setPosition pos
  (ltype, lhs') <- inferExprM lhs
  (rtype, rhs') <- inferExprM rhs
  unifTypes ltype rtype
  exitScopeM
  return $ Equation pos lhs' rhs'

unifTypes :: TypeScheme -> TypeScheme -> M ()
unifTypes = error "NOT IMPLEMENTED"

inferExprM :: Expr -> M (TypeScheme, Expr)
inferExprM = error "NOT IMPLEMENTED"

exprToType :: Expr -> Type
exprToType = error "NOT IMPLEMENTED"

constraintToTypeConstraint :: Constraint -> TypeConstraint
constraintToTypeConstraint = error "NOT IMPLEMENTED"

