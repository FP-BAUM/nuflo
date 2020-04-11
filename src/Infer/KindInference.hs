
module Infer.KindInference(inferKinds) where

import qualified Data.Set as S
import qualified Data.Map as M

import FailState(FailState, getFS, putFS, modifyFS, evalFS, failFS, logFS)
import Error(Error(..), ErrorType(..))
import Position(Position(..), unknownPosition)
import Syntax.Name(QName, operatorArrow, primitiveInt)
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
import Calculus.Kinds(Kind(..), KindVariable, kindIn)

inferKinds :: Program -> Either Error ()
inferKinds program = evalFS (inferKindProgramM program) initialState
  where initialState = KindInferState {
                         statePosition       = unknownPosition
                       , stateNextFresh      = 0
                       , stateDataTypeNames  = S.empty
                       , stateEnvironment    = [M.empty]
                       , stateSubstitution   = M.empty
                       , stateClassTypeKinds = M.empty
                       }

---- Kind inference monad

data KindInferState =
     KindInferState {
       statePosition       :: Position
     , stateNextFresh      :: KindVariable
     , stateDataTypeNames  :: S.Set QName
     , stateEnvironment    :: [M.Map QName Kind] -- Non-empty stack of ribs
     , stateSubstitution   :: M.Map KindVariable Kind
     , stateClassTypeKinds :: M.Map QName Kind
     }

type M = FailState KindInferState

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

addDataTypeM :: QName -> M ()
addDataTypeM dataTypeName =
  modifyFS (\ state -> state {
    stateDataTypeNames = S.insert dataTypeName (stateDataTypeNames state)
  })

isDataTypeM :: QName -> M Bool
isDataTypeM qname = do
  state <- getFS
  return $ S.member qname (stateDataTypeNames state)

freshKind :: M Kind
freshKind = do
  state <- getFS
  putFS (state { stateNextFresh = stateNextFresh state + 1 })
  return $ KVar (stateNextFresh state) 

bindKind :: QName -> Kind -> M ()
bindKind typeName kind = do
  state <- getFS
  let (rib : ribs) = stateEnvironment state in
   if M.member typeName rib
    then failM KindErrorTypeAlreadyDeclared
               ("Type \"" ++ show typeName ++ "\" already declared.")
    else putFS (state { stateEnvironment = M.insert typeName kind rib : ribs })

bindToFreshKind :: QName -> M ()
bindToFreshKind x = do
  k <- freshKind
  bindKind x k

lookupKind :: QName -> M Kind
lookupKind typeName = do
    state <- getFS
    rec (stateEnvironment state)
  where
    rec []           = failM KindErrorTypeUndeclared
                             ("Type \"" ++ show typeName ++ "\" undeclared.")
    rec (rib : ribs) =
     if M.member typeName rib
      then return $ M.findWithDefault undefined typeName rib
      else rec ribs

enterScopeM :: M ()
enterScopeM = modifyFS (\ state -> state {
                stateEnvironment = M.empty : stateEnvironment state
              })

exitScopeM :: M ()
exitScopeM = modifyFS (\ state -> state {
               stateEnvironment = tail (stateEnvironment state)
             })

boundTypeVariablesM :: M (S.Set QName)
boundTypeVariablesM = do
    state <- getFS
    return $ rec (stateEnvironment state)
  where
    rec []           = S.fromList []
    rec (rib : ribs) = M.keysSet rib `S.union` rec ribs

setClassTypeKindM :: QName -> Kind -> M ()
setClassTypeKindM className kind = do
  state <- getFS
  let m = stateClassTypeKinds state in
    if M.member className m
     then failM KindErrorClassAlreadyDeclared
                ("Class \"" ++ show className ++ "\" already declared.")
     else putFS (state { stateClassTypeKinds = M.insert className kind m })

getClassTypeKindM :: QName -> M Kind
getClassTypeKindM className = do
  state <- getFS
  let m = stateClassTypeKinds state in
    if M.member className m
     then return $ M.findWithDefault undefined className m
     else failM KindErrorClassUndeclared
                ("Class \"" ++ show className ++ "\" has not been declared.")

---- Kind inference algorithm

inferKindProgramM :: Program -> M ()
inferKindProgramM (Program decls) = do
  -- Initialize primitive types
  bindKind operatorArrow (KFun KType (KFun KType KType))
  bindKind primitiveInt KType
  -- Infer kinds of all declarations
  mapM_ declareTypeM decls
  mapM_ inferKindDeclarationM decls

-- Split the left-hand side in the definition of a datatype,
-- such as "Map a b" into the head "Map" and the arguments ["a", "b"]
-- checking that it is well-formed.
--
-- All the arguments should be variables.
splitDatatypeArgs :: Expr -> M (QName, [QName])
splitDatatypeArgs (EApp _ f (EVar _ x)) = do
  (head, args) <- splitDatatypeArgs f
  return (head, args ++ [x])
splitDatatypeArgs (EVar _ x) =
  return (x, [])
splitDatatypeArgs expr = do
  setPosition (exprAnnotation expr)
  failM KindErrorMalformedDatatype "Malformed datatype."

declareTypeM :: Declaration -> M ()
declareTypeM (DataDeclaration pos typ _) = do
  setPosition pos
  (name, args) <- splitDatatypeArgs typ
  addDataTypeM name
  argKinds <- mapM (const freshKind) args
  let typeKind = foldr KFun KType argKinds in
    bindKind name typeKind
declareTypeM (TypeDeclaration pos typ _) = do
  setPosition pos
  (name, args) <- splitDatatypeArgs typ
  argKinds <- mapM (const freshKind) args
  resKind  <- freshKind
  let typeKind = foldr KFun resKind argKinds in
    bindKind name typeKind
declareTypeM (ClassDeclaration pos className _ _) = do
  setPosition pos
  classTypeKind <- freshKind
  setClassTypeKindM className classTypeKind
declareTypeM _ = return ()

-- Given a list [x1, ..., xn]
-- and a kind (k1 -> ... -> kn -> k),
-- build a list [(x1, k1), ..., (xn, kn)].
zipKinds :: [QName] -> Kind -> [(QName, Kind)]
zipKinds []       _           = []
zipKinds (x : xs) (KFun k ks) = (x, k) : zipKinds xs ks
zipKinds _        _           = error "(Kind not large enouth for zipKind)"

inferKindDeclarationM :: Declaration -> M ()
inferKindDeclarationM (DataDeclaration pos typ constructors) = do
  setPosition pos
  (name, args) <- splitDatatypeArgs typ
  enterScopeM
  typeKind <- lookupKind name
  mapM_ (uncurry bindKind) (zipKinds args typeKind)
  mapM_ (inferKindConstructorM typ) constructors
  exitScopeM
inferKindDeclarationM (TypeDeclaration pos typ value) = do
  setPosition pos
  (name, args) <- splitDatatypeArgs typ
  enterScopeM
  typeKind <- lookupKind name
  mapM_ (uncurry bindKind) (zipKinds args typeKind)
  k1 <- inferKindM typ
  k2 <- inferKindM value
  unifyKindsM k1 k2
  exitScopeM
inferKindDeclarationM (TypeSignature signature) =
  inferKindSignatureM signature
inferKindDeclarationM (ValueDeclaration equation) =
  inferKindEquationM equation
inferKindDeclarationM (ClassDeclaration pos className typeName methods) = do
  setPosition pos
  enterScopeM
  classTypeKind <- getClassTypeKindM className
  bindKind typeName classTypeKind
  mapM_ inferKindSignatureM methods
  exitScopeM
inferKindDeclarationM (InstanceDeclaration pos className typ
                                           constraints methods) = do
  setPosition pos
  classTypeKind <- getClassTypeKindM className
  (name, args) <- splitDatatypeArgs typ
  b <- isDataTypeM name
  enterScopeM
  typeKind <- lookupKind name
  if b
   then return ()
   else failM KindErrorInstanceMustBeDatatype
              "Type on instance declaration must be a datatype."
  let argsAndKinds = zipKinds args typeKind
  let argKinds = map snd argsAndKinds
  mapM_ (uncurry bindKind) argsAndKinds
  unifyKindsM (foldr KFun classTypeKind argKinds) typeKind
  mapM_ inferKindConstraintM constraints
  exitScopeM
  mapM_ inferKindEquationM methods

inferKindConstructorM :: Expr -> Signature -> M ()
inferKindConstructorM dataType
                      signature@(Signature pos cName cType _) = do
    setPosition pos
    if resultType cType `exprEqual` dataType
     then return ()
     else failM KindErrorConstructorShouldReturnDataType
                ("Constructor \"" ++ show cName ++ "\" is of type:\n" ++
                 "  " ++ show cType ++ "\n" ++
                 "But it should return:\n" ++
                 "  " ++ show dataType)
    inferKindSignatureM signature
  where
    resultType x =
      if exprIsFunctionType x
       then resultType (exprFunctionTypeCodomain x)
       else x

inferKindSignatureM :: Signature -> M ()
inferKindSignatureM (Signature pos _ cType constraints) = do
  setPosition pos
  enterScopeM
  -- Lookup all free type variables and give them fresh kinds
  allTypeVariables   <- typeVariablesM cType
  boundTypeVariables <- boundTypeVariablesM
  let freeTypeVariables = S.difference allTypeVariables boundTypeVariables
  mapM_ bindToFreshKind freeTypeVariables
  -- Check that the type is of base kind (*)
  kind <- inferKindM cType
  unifyKindsM kind KType
  mapM_ inferKindConstraintM constraints
  exitScopeM
  return ()

inferKindConstraintM :: Constraint -> M ()
inferKindConstraintM (Constraint pos className typeName) = do
  setPosition pos
  k1 <- getClassTypeKindM className
  k2 <- lookupKind typeName
  unifyKindsM k1 k2

inferKindEquationM :: Equation -> M ()
inferKindEquationM (Equation pos lhs rhs) = do
  setPosition pos
  inferKindExpressionM lhs
  inferKindExpressionM rhs

inferKindExpressionM :: Expr -> M ()
inferKindExpressionM (EVar _ _)           = return ()
inferKindExpressionM (EInt _ _)           = return ()
inferKindExpressionM (EApp _ f x)         = do inferKindExpressionM f
                                               inferKindExpressionM x
inferKindExpressionM (ELambda _ x b)      = inferKindExpressionM b
inferKindExpressionM (ELet _ ds e)        = do
  mapM_ inferKindDeclarationM ds
  inferKindExpressionM e
inferKindExpressionM (ECase _ e branches) = do
  inferKindExpressionM e
  mapM_ inferKindBranchM branches
inferKindExpressionM (EFresh _ _ e)       = inferKindExpressionM e

inferKindBranchM :: CaseBranch -> M ()
inferKindBranchM (CaseBranch _ p e) = do
  inferKindExpressionM p
  inferKindExpressionM e

inferKindM :: Expr -> M Kind
inferKindM (EVar _ qname) = lookupKind qname
inferKindM (EApp pos f x)   = do
  setPosition pos
  kr <- freshKind
  kf <- inferKindM f
  kx <- inferKindM x
  unifyKindsM kf (KFun kx kr)
  return kr
inferKindM expr           = do
  setPosition (exprAnnotation expr)
  failM KindErrorMalformedType ("Malformed type: " ++ show expr)

typeVariablesM :: Expr -> M (S.Set QName)
typeVariablesM (EVar _ qname) = return (S.fromList [qname])
typeVariablesM (EApp _ f x)   = do
  vf <- typeVariablesM f
  vx <- typeVariablesM x
  return (vf `S.union` vx)
typeVariablesM expr           = do
  setPosition (exprAnnotation expr)
  failM KindErrorMalformedType ("Malformed type: " ++ show expr)

---- Unification algorithm

unfoldKind :: Kind -> M Kind
unfoldKind (KVar x) = do
  state <- getFS
  let sub = stateSubstitution state in
    if M.member x sub
     then unfoldKind (M.findWithDefault undefined x sub)
     else return $ KVar x
unfoldKind KType        = return KType
unfoldKind (KFun k1 k2) = do
  k1' <- unfoldKind k1
  k2' <- unfoldKind k2
  return $ KFun k1' k2'

instantiateKindVariable :: KindVariable -> Kind -> M ()
instantiateKindVariable x k = modifyFS (\ state -> state {
                                stateSubstitution =
                                  M.insert x k (stateSubstitution state)
                              })

unifyKindsM :: Kind -> Kind -> M ()
unifyKindsM k1 k2 = do
  k1' <- unfoldKind k1
  k2' <- unfoldKind k2
  case (k1', k2') of
    (KVar x, KVar y) | x == y -> return ()
    (KVar x, _) | x `kindIn` k2' -> failOccursCheck k1' k2'
    (KVar x, _)                  -> instantiateKindVariable x k2'
    (_, KVar x) | x `kindIn` k1' -> failOccursCheck k1' k2'
    (_, KVar x)                  -> instantiateKindVariable x k1'
    (KType, KType)               -> return ()
    (KFun a1 b1, KFun a2 b2)     -> do unifyKindsM a1 a2
                                       unifyKindsM b1 b2
    _                            -> failClash k1' k2'
  where
    failOccursCheck k1' k2' =
      failM KindErrorUnifyOccursCheck
        ("Occurs check fail when unifying kinds:\n" ++
         "  " ++ show k1' ++ "\n" ++
         "  " ++ show k2' ++ "\n")
    failClash k1' k2' =
      failM KindErrorUnifyClash
        ("Kinds do not match:\n" ++
         "  " ++ show k1' ++ "\n" ++
         "  " ++ show k2' ++ "\n")

