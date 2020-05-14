module Infer.TypeInference(inferTypes) where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.List(union)
import Data.Maybe(fromJust)

import Error(Error(..), ErrorType(..))
import FailState(FailState, getFS, putFS, modifyFS, evalFS, failFS, logFS)
import Position(Position(..), unknownPosition)
import Syntax.Name(QName(..), primitiveArrow, primitiveInt, unqualifiedName)
import Syntax.AST(
         AnnProgram(..), Program,
         AnnDeclaration(..), Declaration,
         AnnSignature(..), Signature,
         AnnEquation(..), Equation,
         AnnConstraint(..), Constraint,
         AnnCaseBranch(..), CaseBranch,
         AnnExpr(..), Expr, exprHeadVariable, exprFreeVariables,
         exprFunctionType, splitDatatypeArgsOrFail
       )
import Calculus.Types(
         TypeMetavariable, TypeConstraint(..),
         TypeScheme(..),  ConstrainedType(..), Type(..),
         substituteConstrainedType,
         constrainedTypeFreeVariables,
         typeSchemeMetavariables,
         tFun, tInt
       )
import Infer.Utils(mergeEquations)

inferTypes :: Program -> Either Error Program
inferTypes program = evalFS (inferTypeProgramM program) initialState
  where initialState = TypeInferState {
                         statePosition        = unknownPosition
                       , stateNextFresh       = 0
                       , stateTypeConstants   = S.empty
                       , stateTypeSynonyms    = M.empty
                       , stateEnvironment     = [M.empty]
                       , stateSubstitution    = M.empty
                       , stateTypeVarRenaming = M.empty
                       }

---- Type inference monad

data TypeInferState =
     TypeInferState {
       statePosition        :: Position
     , stateNextFresh       :: Integer
     , stateTypeConstants   :: S.Set QName  -- Type constructors and synonyms
     , stateTypeSynonyms    :: TypeSynonymTable
     , stateEnvironment     :: [M.Map QName TypeScheme]    -- Non-empty stack
     , stateSubstitution    :: M.Map TypeMetavariable Type
     , stateTypeVarRenaming :: M.Map QName QName
     }
type TypeSynonymTable = M.Map QName ([QName], Type)

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

freshType :: M Type
freshType = do
  state <- getFS
  putFS (state { stateNextFresh = stateNextFresh state + 1 })
  return $ TMetavar (stateNextFresh state)

freshTypeVar :: M Type
freshTypeVar = do
  state <- getFS
  putFS (state { stateNextFresh = stateNextFresh state + 1 })
  return $ TVar $ Name ("t{" ++ show (stateNextFresh state) ++ "}")

overrideType :: QName -> TypeScheme -> M ()
overrideType varName typ = do
  state <- getFS
  let (rib : ribs) = stateEnvironment state in
   putFS (state { stateEnvironment = M.insert varName typ rib : ribs })

bindType :: QName -> TypeScheme -> M ()
bindType varName typ = do
  state <- getFS
  let (rib : ribs) = stateEnvironment state in
   if M.member varName rib
    then failM TypeErrorVariableAlreadyDeclared
               ("Variable \"" ++ show varName ++ "\" already declared.")
    else putFS (state { stateEnvironment = M.insert varName typ rib : ribs })

setRepresentative :: TypeMetavariable -> Type -> M ()
setRepresentative x typ = do
  state <- getFS
  putFS (state {
           stateSubstitution = M.insert x typ (stateSubstitution state)
         })

bindToFreshType :: QName -> M ()
bindToFreshType x = do
  typ <- freshType
  bindType x (TypeScheme [] (ConstrainedType [] typ))

bindToFreshTypeIfNotLocallyBound :: QName -> M ()
bindToFreshTypeIfNotLocallyBound x = do
  state <- getFS
  if M.member x (head (stateEnvironment state))
   then return ()
   else bindToFreshType x

lookupType :: QName -> M TypeScheme
lookupType x = do
    state <- getFS
    rec (stateEnvironment state)
  where
    rec []           =
      failM TypeErrorUnboundVariable
            ("Unbound variable \"" ++ show x ++ "\"")
    rec (rib : ribs) =
      if M.member x rib
       then return $ M.findWithDefault undefined x rib
       else rec ribs

getAllBoundVars :: M (S.Set QName)
getAllBoundVars = do
  state <- getFS
  return $ S.unions (map M.keysSet (stateEnvironment state))

enterScopeM :: M ()
enterScopeM = modifyFS (\ state -> state {
                stateEnvironment = M.empty : stateEnvironment state
              })

getEnvironment :: M [M.Map QName TypeScheme]
getEnvironment = do
  state <- getFS
  return $ stateEnvironment state

exitScopeM :: M ()
exitScopeM = modifyFS (\ state -> state {
               stateEnvironment = tail $ stateEnvironment state
             })

allTypeConstants :: M (S.Set QName)
allTypeConstants = do
  state <- getFS
  return $ stateTypeConstants state

isTypeConstant :: QName -> M Bool
isTypeConstant name = do
  constants <- allTypeConstants
  return $ S.member name constants

addTypeConstant :: QName -> M ()
addTypeConstant name =
  modifyFS (\ state ->
    state { stateTypeConstants = S.insert name (stateTypeConstants state) }
  )

freshenVariables :: [QName] -> ConstrainedType -> M ConstrainedType
freshenVariables names constrainedType = do
  sub <- M.fromList <$> mapM (\ name -> do ft <- freshType 
                                           return (name, ft)) names
  constrainedType' <- unfoldConstrainedType constrainedType
  return $ substituteConstrainedType sub constrainedType'

lookupMetavar :: TypeMetavariable -> M (Maybe Type)
lookupMetavar meta = do
  state <- getFS
  return $ M.lookup meta (stateSubstitution state)

defineTypeSynonym :: QName -> [QName] -> Type -> M ()
defineTypeSynonym name args value =
  modifyFS (\ state ->
    state { stateTypeSynonyms = M.insert name (args, value)
                                              (stateTypeSynonyms state) }
  )

cleanTypeVariableRenaming :: M ()
cleanTypeVariableRenaming = do
  state <- getFS
  putFS (state { stateTypeVarRenaming = M.empty })

setNewTypeVariableName :: QName -> QName -> M ()
setNewTypeVariableName name name' = do
  modifyFS (\ state ->
    state { stateTypeVarRenaming =
              M.insert name name' (stateTypeVarRenaming state) })

getNewTypeVariableName :: QName -> M QName
getNewTypeVariableName name = do
  typeVarRenaming <- stateTypeVarRenaming <$> getFS
  if M.member name typeVarRenaming
   then return $ M.findWithDefault undefined name typeVarRenaming
   else do
     tvar <- freshTypeVar
     case tvar of
       TVar name' -> do setNewTypeVariableName name name'
                        return name'
       _          -> error "(Fresh type var is not a variable name)"

---- Utility functions to convert between expressions
---- in the AST and proper types

exprToType :: Expr -> Type
exprToType (EVar _ x)     = TVar x
exprToType (EApp _ t1 t2) = TApp (exprToType t1) (exprToType t2)
exprToType _              = error "(Malformed type)"

typeToExpr :: Position -> Type -> Expr
typeToExpr pos (TVar x)     = EVar pos x
typeToExpr pos (TApp t1 t2) = EApp pos (typeToExpr pos t1) (typeToExpr pos t2)
typeToExpr _   _            = error "(Malformed type)"

constraintToTypeConstraint :: Constraint -> TypeConstraint
constraintToTypeConstraint (Constraint _ className typeName) =
  TypeConstraint className (TVar typeName)

typeConstraintToConstraint :: Position -> TypeConstraint -> Constraint
typeConstraintToConstraint pos (TypeConstraint className typ) =
  let (EVar _ name) = typeToExpr pos typ
   in Constraint pos className name

---- Name mangling to avoid collisions with user names

mangleClassDataType :: QName -> QName
mangleClassDataType className = Name ("class" ++ "{" ++ show className ++ "}")

mangleClassConstructor :: QName -> QName
mangleClassConstructor className = Name ("mk" ++ "{" ++ show className ++ "}")

---- Type inference algorithm

inferTypeProgramM :: Program -> M Program
inferTypeProgramM (Program decls) = do
  -- Declare built-in type constructors
  addTypeConstant primitiveArrow
  addTypeConstant primitiveInt
  -- Infer
  mapM_ collectTypeDeclarationM decls
  mapM_ collectSignaturesM decls
  decls' <- inferTypeDeclarationsM decls
  return $ Program decls'

collectTypeDeclarationM :: Declaration -> M ()
collectTypeDeclarationM (TypeDeclaration pos typ value) = do
  case exprHeadVariable typ of
    Just name -> addTypeConstant name
    _ -> error "Type has no head variable"
  let (name, args) = splitDatatypeArgsOrFail typ in
    defineTypeSynonym name args (exprToType value)
collectTypeDeclarationM (DataDeclaration _ typ _) = do
  case exprHeadVariable typ of
    Just name -> addTypeConstant name
    _ -> error "Type has no head variable"
collectTypeDeclarationM _ = return ()

collectSignaturesM :: Declaration -> M ()
collectSignaturesM (DataDeclaration pos typ constructors) = do
  mapM_ collectSignatureM constructors
collectSignaturesM (TypeSignature signature) = collectSignatureM signature
collectSignaturesM (ClassDeclaration pos className typeName methods) = do
    mapM_ collectMethodSignature methods
  where
    collectMethodSignature :: Signature -> M ()
    collectMethodSignature (Signature pos name typ cs) = do
      setPosition pos
      if (any (\(Constraint _ cClass ctype) -> typeName == ctype) cs)
       then failM ClassErrorConstrainedParameter
              ("Class parameter " ++ show typeName ++ " cannot be constrained")
       else -- Add class constraint
            let c = Constraint pos className typeName in
              collectSignatureM (Signature pos name typ (c : cs))
collectSignaturesM _ = return ()

inferTypeDeclarationsM :: [Declaration] -> M [Declaration]
inferTypeDeclarationsM decls =
    let definedVars = S.unions (map declVars decls)
     in do mapM_ bindToFreshTypeIfNotLocallyBound (S.toList definedVars)
           mapM inferTypeDeclarationM decls
  where
    declVars (ValueDeclaration (Equation _ lhs _)) =
      S.fromList [fromJust (exprHeadVariable lhs)]
    declVars _ = S.empty

inferTypeDeclarationM :: Declaration -> M Declaration
inferTypeDeclarationM decl@(DataDeclaration _ _ _) =
  -- TODO: transform constraints in constructor signatures
  return decl
inferTypeDeclarationM decl@(TypeDeclaration _ _ _) =
  -- TODO: transform constraints in constructor signatures
  return decl
inferTypeDeclarationM (ValueDeclaration equation) = do
  equation' <- inferTypeEquationM equation
  return $ ValueDeclaration equation'
inferTypeDeclarationM decl@(TypeSignature _) = 
  -- TODO: transform constraints in signature
  return decl
inferTypeDeclarationM (ClassDeclaration pos className typeName methods) =
  inferTypeClassDeclarationM pos className typeName methods
inferTypeDeclarationM (InstanceDeclaration pos className typ constraints methods) =
  -- instance Eq Bool where
  -- igual    = igualBool
  -- enumerar = enumBool
  --  =>
  -- igualBool{Eq}{Bool} = ...
  -- enumBool{Eq}{Bool} =  = ...
  -- instance{Eq}{Bool} = mk{Eq} igualBool{Eq}{Bool} enumBool{Eq}{Bool}
  error "NOT IMPLEMENTED"

collectSignatureM :: Signature -> M ()
collectSignatureM (Signature pos name typ constraints) = do 
  setPosition pos
  ct <- constrainedType constraints typ
  cs <- allTypeConstants
  let fvariables = S.toList $ constrainedTypeFreeVariables ct S.\\ cs
  bindType name (TypeScheme fvariables ct)

constrainedType :: [Constraint] -> Expr -> M ConstrainedType
constrainedType constraints expr =
    return $ ConstrainedType cts typ
  where
    typ = exprToType expr
    cts = map constraintToTypeConstraint constraints

inferTypeEquationM :: Equation -> M Equation
inferTypeEquationM (Equation pos lhs rhs) = do
  setPosition pos
  bound <- getAllBoundVars
  let lhsFree = exprFreeVariables bound lhs
      --rhsFree = exprFreeVariables (bound `S.union` lhsFree) rhs
      --allFree = lhsFree `S.union` rhsFree
   in do
     -- TODO: transform constraints
     enterScopeM
     mapM_ bindToFreshType lhsFree
     (ConstrainedType tcsl tl, lhs') <- inferTypeExprM lhs
     (ConstrainedType tcsr tr, rhs') <- inferTypeExprM rhs
     unifyTypes tl tr
     solveConstraints (union tcsl tcsr)
     exitScopeM
     return $ Equation pos lhs' rhs'

representative :: Type -> M Type
representative (TMetavar x) = do
  mt <- lookupMetavar x
  case mt of
    Just t  -> representative t
    Nothing -> return (TMetavar x)
representative t            = return t

-- Normalize a type to weak head normal form (using type synonyms).
weaklyHeadNormalizeType :: Type -> M Type
weaklyHeadNormalizeType typ = do
    table <- stateTypeSynonyms <$> getFS
    typ'  <- unfoldType typ
    rec table [] typ'
  where
    rec table forbidden typ = do
      let (head, args) = splitHeadArgs typ
      case head of
        TVar f | canApplyT table f args
               -> if f `elem` forbidden
                   then failM TypeErrorSynonymLoop
                              ("Loop in type synonyms. Chain of calls:\n" ++
                               unlines (map (\ x -> "  " ++ show x)
                                            (reverse forbidden)))
                   else rec table (f : forbidden) (applyT table f args)
        _      -> return $ foldl TApp head args

    splitHeadArgs :: Type -> (Type, [Type])
    splitHeadArgs (TMetavar x) = (TMetavar x, [])
    splitHeadArgs (TVar x)     = (TVar x, [])
    splitHeadArgs (TApp t1 t2) =
      let (head, args) = splitHeadArgs t1
       in (head, args ++ [t2])

    canApplyT :: TypeSynonymTable -> QName -> [Type] -> Bool
    canApplyT table f args =
      M.member f table &&
      let (params, _) = M.findWithDefault undefined f table in
        length args >= length params

    applyT :: TypeSynonymTable -> QName -> [Type] -> Type
    applyT table f args =
      let (params, value)        = M.findWithDefault undefined f table
          (args', remainingArgs) = splitAt (length params) args
       in foldl TApp
                (instantiateT value (M.fromList (zip params args)))
                remainingArgs

    instantiateT :: Type -> M.Map QName Type -> Type
    instantiateT (TMetavar x) _   = TMetavar x
    instantiateT (TVar x)     sub = M.findWithDefault (TVar x) x sub
    instantiateT (TApp t s)   sub = TApp (instantiateT t sub)
                                         (instantiateT s sub)

generalizableMetavariables :: M [TypeMetavariable]
generalizableMetavariables = do
    rib  <- head <$> getEnvironment
    ribs <- tail <$> getEnvironment
    localMetavars <- getMetavars rib
    fixedMetavars <- S.unions <$> mapM getMetavars ribs
    return $ S.toList (S.difference localMetavars fixedMetavars)
  where
    getMetavars :: M.Map QName TypeScheme -> M (S.Set TypeMetavariable)
    getMetavars rib = do
      schemes <- mapM unfoldTypeScheme (M.elems rib)
      return $ S.unions (map typeSchemeMetavariables schemes)

unfoldTypeScheme :: TypeScheme -> M TypeScheme
unfoldTypeScheme (TypeScheme names constrainedType) = do
  constrainedType' <- unfoldConstrainedType constrainedType
  return $ TypeScheme names constrainedType'

unfoldConstrainedType :: ConstrainedType -> M ConstrainedType
unfoldConstrainedType (ConstrainedType typeConstraints typ) = do
  typeConstraints' <- mapM unfoldTypeConstraint typeConstraints
  typ' <- unfoldType typ
  return $ ConstrainedType typeConstraints' typ'

unfoldTypeConstraint :: TypeConstraint -> M TypeConstraint
unfoldTypeConstraint (TypeConstraint name typ) = do
  typ' <- unfoldType typ
  return $ TypeConstraint name typ'

generalizeLocalVar :: [Type] -> QName -> TypeScheme -> M ()
generalizeLocalVar genVars x (TypeScheme names constrainedType) = do
  overrideType x (TypeScheme ((map unVar genVars) ++ names) constrainedType)
  where
    unVar (TVar x) = x
    unVar _        = error "(Generalized type must be a type variable)"

generalizeLocalVars :: M ()
generalizeLocalVars = do
  genMetavars <- generalizableMetavariables
  genVars <- mapM (const freshTypeVar) genMetavars
  mapM_ (uncurry setRepresentative) (zip genMetavars genVars)
  rib <- head <$> getEnvironment
  mapM_ (uncurry $ generalizeLocalVar genVars) (M.toList rib)

unfoldType :: Type -> M Type
unfoldType t = do
  t' <- representative t
  case t' of
    TApp t1 t2 -> do
      t1' <- unfoldType t1
      t2' <- unfoldType t2
      return $ TApp t1' t2'
    _          -> return t'

occursIn :: TypeMetavariable -> Type -> M Bool
occursIn x t = do
  t' <- representative t
  case t' of
    TMetavar y -> return (x == y)
    TVar _     -> return False
    TApp t1 t2 -> do
      b1 <- occursIn x t1
      b2 <- occursIn x t2
      return (b1 || b2)

solveConstraints :: [TypeConstraint] -> M [TypeConstraint]
solveConstraints x = return x -- TODO: NOT IMPLEMENTED

unifyTypes :: Type -> Type -> M ()
unifyTypes t1 t2 = do
  t1' <- weaklyHeadNormalizeType t1
  t2' <- weaklyHeadNormalizeType t2
  case (t1', t2') of
    (TMetavar x, TMetavar y) | x == y -> return ()
    (TMetavar x, t) -> do
      b <- x `occursIn` t
      if b
       then unifFailOccursCheck t1 t2
       else setRepresentative x t
    (t, TMetavar x) -> unifyTypes (TMetavar x) t
    (TVar a, TVar b) | a == b -> return ()
    (TApp t11 t12, TApp t21 t22) -> do unifyTypes t11 t21
                                       unifyTypes t12 t22
    _ -> unifFailClash t1 t2
  where
    unifFailOccursCheck = unifFail TypeErrorUnificationOccursCheck
    unifFailClash       = unifFail TypeErrorUnificationClash
    unifFail errorType t1 t2 = do
      t1' <- unfoldType t1
      t2' <- unfoldType t2
      failM errorType
            ("Types do not unify (" ++ show errorType ++ "):\n" ++
             "  " ++ show t1' ++ "\n" ++
             "  " ++ show t2')

type InferResult = (ConstrainedType, Expr)

inferTypeExprM :: Expr -> M InferResult
inferTypeExprM (EInt pos n)               = inferTypeEIntM pos n
inferTypeExprM (EVar pos x)               = inferTypeEVarM pos x
inferTypeExprM (EUnboundVar pos x)        = inferTypeEVarM pos x
inferTypeExprM (EApp pos e1 e2)           = inferTypeEAppM pos e1 e2
inferTypeExprM (ELambda pos e1 e2)        = inferTypeELambdaM pos e1 e2
inferTypeExprM (ELet pos decls body)      = inferTypeELetM pos decls body
inferTypeExprM (ECase pos guard branches) = inferTypeECaseM pos guard branches
inferTypeExprM (EFresh pos x body)        = inferTypeEFreshM pos x body

-- Integer constant (n)
inferTypeEIntM :: Position -> Integer -> M InferResult
inferTypeEIntM pos n = return (ConstrainedType [] tInt, EInt pos n)

-- Variable (x)
inferTypeEVarM :: Position -> QName -> M InferResult
inferTypeEVarM pos x = do
  setPosition pos
  TypeScheme gvars constrainedType <- lookupType x
  constrainedType' <- freshenVariables gvars constrainedType -- Instantiate
  return (constrainedType', EVar pos x)

-- e1 e2
inferTypeEAppM :: Position -> Expr -> Expr -> M InferResult
inferTypeEAppM pos e1 e2 = do
  (ConstrainedType c1 t1, e1') <- inferTypeExprM e1
  (ConstrainedType c2 t2, e2') <- inferTypeExprM e2
  tr <- freshType
  setPosition pos
  unifyTypes t1 (tFun t2 tr)
  constraints <- solveConstraints (union c1 c2)
  return (ConstrainedType constraints tr, (EApp pos e1' e2'))

-- \ e1 -> e2
inferTypeELambdaM :: Position -> Expr -> Expr -> M InferResult
inferTypeELambdaM pos e1 e2 = do
  bound <- getAllBoundVars
  let freeParamVars = exprFreeVariables bound e1
   in do
    enterScopeM
    mapM_ bindToFreshType freeParamVars
    (ConstrainedType ce1 te1, e1') <- inferTypeExprM e1
    (ConstrainedType ce2 te2, e2') <- inferTypeExprM e2
    exitScopeM
    return $ (ConstrainedType (union ce1 ce2) (tFun te1 te2),
              ELambda pos e1' e2')

-- let decls in body
inferTypeELetM :: Position -> [Declaration] -> Expr -> M InferResult
inferTypeELetM pos decls body = do
  setPosition pos
  bound <- getAllBoundVars
  enterScopeM
  mapM_ collectSignaturesM decls
  decls' <- inferTypeDeclarationsM decls
  generalizeLocalVars
  (typeScheme, body') <- inferTypeExprM body
  exitScopeM
  return (typeScheme, ELet pos decls' body')

-- case guard of branches
inferTypeECaseM :: Position -> Expr -> [CaseBranch] -> M InferResult
inferTypeECaseM pos guard branches = do
  setPosition pos
  (ConstrainedType cguard tguard, guard') <- inferTypeExprM guard 
  tresult  <- freshType
  inferredBranches <- mapM (inferCaseBranchM tguard tresult) branches
  let tbranches = map fst inferredBranches
  let cbranches = concatMap (\ (ConstrainedType cns _) -> cns) tbranches
  let branches' = map snd inferredBranches
  constraints <- solveConstraints (cguard ++ cbranches)
  return (ConstrainedType constraints tresult,
          ECase pos guard' branches')

inferCaseBranchM :: Type -> Type -> CaseBranch
                 -> M (ConstrainedType, CaseBranch)
inferCaseBranchM tguard tresult (CaseBranch pos pattern body) = do
  bound <- getAllBoundVars
  let freeParamVars = exprFreeVariables bound pattern
  enterScopeM
  mapM_ bindToFreshType freeParamVars
  (ConstrainedType cpattern tpattern, pattern') <- inferTypeExprM pattern
  setPosition pos
  unifyTypes tguard tpattern
  (ConstrainedType cbody tbody, body') <- inferTypeExprM body
  setPosition pos
  unifyTypes tresult tbody
  exitScopeM
  return $ (ConstrainedType (union cbody cpattern) tbody,
            CaseBranch pos pattern' body')

-- fresh x in body
inferTypeEFreshM :: Position -> QName -> Expr -> M InferResult 
inferTypeEFreshM pos x body = do
  enterScopeM
  bindToFreshType x
  (schema, body') <- inferTypeExprM body
  exitScopeM
  return (schema, EFresh pos x body')

-----

inferTypeClassDeclarationM :: Position -> QName -> QName -> [Signature] ->
                              M Declaration
inferTypeClassDeclarationM pos className typeName methods = do
    constrainedTypes <- mapM renameMethodSignature methods
    let constraints      = concatMap
                             (\ (ConstrainedType cs _) ->
                               map (typeConstraintToConstraint pos) cs)
                             constrainedTypes
        signatureTypes   = map (\ (ConstrainedType _ typ) ->
                                 typeToExpr pos typ)
                               constrainedTypes
        classDatatype    = EVar pos (mangleClassDataType className)
        classDatatype'   = EApp pos classDatatype (EVar pos typeName)
        classConstructor = mangleClassConstructor className
        constructorType  = foldr exprFunctionType classDatatype' signatureTypes
        constructorSig   = Signature pos classConstructor
                                         constructorType
                                         constraints
      in return $ DataDeclaration pos classDatatype' [constructorSig]
  where
    -- Rename all the type variables in the method signature to fresh names,
    -- except for the class parameter (and constants).
    renameMethodSignature :: Signature -> M ConstrainedType
    renameMethodSignature (Signature pos methodName typ cs) = do
      -- TODO: declare method name
      cleanTypeVariableRenaming
      typ' <- renameType (exprToType typ)
      cs'  <- mapM (renameConstraint . constraintToTypeConstraint) cs
      return $ ConstrainedType cs' typ'
      where
        renameType :: Type -> M Type
        renameType (TVar var) = TVar <$> renameTypeVariable var
        renameType (TApp t1 t2) = do
          t1' <- renameType t1
          t2' <- renameType t2
          return $ TApp t1' t2'
        renameType meta@(TMetavar _) = return meta

        renameConstraint :: TypeConstraint -> M TypeConstraint
        renameConstraint (TypeConstraint classN typ) = do
          typ' <- renameType typ
          return $ TypeConstraint classN typ'

        renameTypeVariable :: QName -> M QName
        renameTypeVariable var = do
          b <- isTypeConstant var
          if var == typeName || b
           then return var
           else getNewTypeVariableName var

