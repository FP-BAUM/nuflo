module Infer.TypeInference(inferTypes) where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.List(union)
import Data.Maybe(fromJust)

import Error(Error(..), ErrorType(..))
import FailState(FailState, getFS, putFS, modifyFS, evalFS, failFS, logFS)
import Position(Position(..), unknownPosition)
import Syntax.Name(
         QName(..), primitiveArrow, primitiveInt,
         unqualifiedName,
         primitiveUnit,
         primitiveAlternative, primitiveSequence, primitiveUnification,
         primitivePrint, primitiveUnderscore
       )
import Syntax.AST(
         AnnProgram(..), Program,
         AnnDeclaration(..), Declaration,
         AnnSignature(..), Signature,
         AnnEquation(..), Equation,
         AnnConstraint(..), Constraint,
         AnnCaseBranch(..), CaseBranch,
         AnnExpr(..), Expr,
         unEVar, PlaceholderId,
         exprAnnotation,
         exprHeadVariable, exprHeadArguments,
         exprFreeVariables, exprFunctionType, splitDatatypeArgsOrFail,
         unfoldPlaceholders
       )
import Calculus.Types(
         TypeMetavariable, TypeConstraint(..),
         TypeScheme(..),  ConstrainedType(..), Type(..),
         TypeSubstitution(..),
         substituteConstrainedType,
         substituteType,
         typeSchemeMetavariables, typeSchemeFreeVariables,
         constrainedTypeFreeVariables,
         tFun, tInt, typeHead, typeArgs, unTVar
       )
import Syntax.GroupEquations(groupEquations)

inferTypes :: Program -> Either Error Program
inferTypes program = evalFS (inferTypeProgramM program) initialState
  where initialState = TypeInferState {
                         statePosition              = unknownPosition
                       , stateNextFresh             = 0
                       , stateTypeConstants         = S.empty
                       , stateTypeSynonyms          = M.empty
                       , stateEnvironment           = [M.empty]
                       , stateSubstitution          = M.empty
                       , stateClassParameters       = M.empty
                       , stateClassMethodSignatures = M.empty
                       , stateTypeVarRenaming       = M.empty
                       ---- Constraints
                       , stateMethodInfo            = M.empty
                       , stateFreshPlaceholder      = 0
                       , stateGlobalInstances       = M.empty
                       , stateConstraintEnv         = M.empty
                       , statePlaceholderHeap       = M.empty
                       }

---- Type inference monad

--
-- stateGlobalInstances:
--   Has the rules given by instance declarations.
--   For example, the instance declaration
--   "instance Eq (List a b) {Eq a, Show b}"
--   corresponds to an entry
--     (<Eq>, <List>) ~~>
--       GlobalInstance <Eq> <List> [<a>, <b>] [(<Eq>, <a>), (<Show>, <b>)].
--
-- stateConstraintEnv:
--   Has the placeholders for the current pending constraints.
--   For example, each time we use a function:
--       f : a -> a -> String   {Show a, Eq a}
--   Its type variables are instantiated in fresh metavariables:
--       <?1> -> <?1> -> String
--   Two new instance placeholders are created and set in this map:
--       (<Show>, <?1>) ~~> PLACEHOLDER_91
--       (<Eq>, <?1>)   ~~> PLACEHOLDER_92
--   And the resulting expression is:
--       (f PLACEHOLDER_91 PLACEHOLDER_92)

data TypeInferState =
     TypeInferState {
       statePosition              :: Position
     , stateNextFresh             :: Integer
     -- stateTypeConstants has all the type constructors and synonyms
     , stateTypeConstants         :: S.Set QName
     , stateTypeSynonyms          :: TypeSynonymTable
     -- stateEnvironment is a non-empty stack of ribs
     , stateEnvironment           :: [M.Map QName TypeScheme]
     , stateSubstitution          :: M.Map TypeMetavariable Type
     , stateClassParameters       :: M.Map QName QName
     , stateClassMethodSignatures :: M.Map QName (M.Map QName Signature)
     , stateTypeVarRenaming       :: M.Map QName QName
     ---- Constraints
     , stateMethodInfo            :: M.Map QName MethodInfo
     , stateFreshPlaceholder      :: Integer
     , stateGlobalInstances       :: M.Map (QName, QName) GlobalInstance
     , stateConstraintEnv         :: M.Map (QName, TypeMetavariable)
                                           PlaceholderId
     , statePlaceholderHeap       :: M.Map PlaceholderId Expr
     }
type TypeSynonymTable = M.Map QName ([QName], Type)
data MethodInfo = MethodInfo { methodInfoClassName :: QName
                             , methodSignature     :: Signature
                             }
data GlobalInstance = GlobalInstance {
                        globalInstanceClass           :: QName
                      , globalInstanceTypeConstructor :: QName
                      , globalInstanceTypeParameters  :: [QName]
                      , globalInstanceConstraints     :: [(QName, QName)]
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

freshEVar :: M Expr
freshEVar = do
  state <- getFS
  pos <- currentPosition
  putFS (state { stateNextFresh = stateNextFresh state + 1 })
  return $ EVar pos (Name ("x{" ++ show (stateNextFresh state) ++ "}"))

bindType :: QName -> TypeScheme -> M ()
bindType varName typ = do
  state <- getFS
  let (rib : ribs) = stateEnvironment state in
   if M.member varName rib
    then failM TypeErrorVariableAlreadyDeclared
               ("Variable \"" ++ show varName ++ "\" already declared.")
    else putFS (state { stateEnvironment = M.insert varName typ rib : ribs })

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

overrideType :: QName -> TypeScheme -> M ()
overrideType varName typ = do
  state <- getFS
  let (rib : ribs) = stateEnvironment state in
   putFS (state { stateEnvironment = M.insert varName typ rib : ribs })

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

getAllBoundVars :: M (S.Set QName)
getAllBoundVars = do
  state <- getFS
  return $ S.unions (map M.keysSet (stateEnvironment state))

enterScopeM :: M ()
enterScopeM = modifyFS (\ state -> state {
                stateEnvironment   = M.empty : stateEnvironment state
              })

exitScopeM :: M ()
exitScopeM = modifyFS (\ state -> state {
               stateEnvironment   = tail $ stateEnvironment state
             })

getEnvironment :: M [M.Map QName TypeScheme]
getEnvironment = do
  state <- getFS
  return $ stateEnvironment state

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

freshenVariables :: [QName] -> ConstrainedType
                 -> M ([PlaceholderId], ConstrainedType)
freshenVariables genVarNames constrainedType = do
  sub <- M.fromList <$> mapM (\ name -> do ft <- freshType 
                                           return (name, ft))
                             genVarNames
  constrainedType' <- unfoldConstrainedType constrainedType
  freshenConstrainedType sub constrainedType'
  where
    freshenConstrainedType :: TypeSubstitution ->  ConstrainedType
                           -> M ([PlaceholderId], ConstrainedType)
    freshenConstrainedType sub (ConstrainedType typeConstraints typ) = do
      (plhs, remainingConstraints) <-
        freshenConstraints sub typeConstraints
      return (plhs,
              ConstrainedType remainingConstraints (substituteType sub typ))
    
    unMetaVar :: Type -> TypeMetavariable
    unMetaVar (TMetavar metavar) = metavar
    unMetaVar _ = error "(Not a meta variable)"

    -- Given a substitution mapping (some) type variables to fresh type
    -- metavariables, and given a list of type constraints,
    -- split the list of type constraints into:
    -- - Those constraining variables refreshed by the substitution.
    --   For each of these we create a new placeholder for the instance.
    -- - Those constraining other variables or metavariables.
    --   These are kept and returned in the second element of the result.
    freshenConstraints :: TypeSubstitution
                       -> [TypeConstraint]
                       -> M ([PlaceholderId], [TypeConstraint])
    freshenConstraints _   [] = return ([], [])
    freshenConstraints sub (TypeConstraint className typ : tcs) = do
      (plhs, remainingConstraints) <- freshenConstraints sub tcs
      case typ of
        TVar typeName | M.member typeName sub -> do
          let metavar = unMetaVar $ M.findWithDefault undefined typeName sub
          plh <- freshPlaceholder
          setMetavarPlaceholder className metavar plh
          return (plh : plhs, remainingConstraints)
        _  -> return (plhs,
                      (TypeConstraint className typ) : remainingConstraints)

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

addClassMethodSignature :: QName -> Signature -> M ()
addClassMethodSignature className methodSignature = do
  state <- getFS
  let ms = stateClassMethodSignatures state
  putFS (state {
    stateClassMethodSignatures =
      M.insert className
               (M.insert (signatureName methodSignature)
                         methodSignature
                         (M.findWithDefault M.empty className ms))
               ms
    ,
    stateMethodInfo =
      M.insert (signatureName methodSignature)
               (MethodInfo className methodSignature)
               (stateMethodInfo state)
  })

getClassMethodNames :: QName -> M [QName]
getClassMethodNames className = do
  state <- getFS
  return $ M.keys
         $ M.findWithDefault
             M.empty
             className
             (stateClassMethodSignatures state)

getClassMethodSignature :: QName -> QName -> M Signature
getClassMethodSignature className methodName = do
  state <- getFS
  let ms = M.findWithDefault M.empty className
                             (stateClassMethodSignatures state)
  if M.member methodName ms
   then return $ M.findWithDefault undefined methodName ms
   else failM TypeErrorUndefinedClassMethod
              ("Undefined class method \"" ++ show methodName ++ "\". ")

isMethod :: QName -> M Bool
isMethod methodName = do
  state <- getFS
  return $ M.member methodName (stateMethodInfo state)

setClassParameter :: QName -> QName -> M ()
setClassParameter className parameter =
  modifyFS (\ state -> state {
    stateClassParameters = M.insert className parameter
                                    (stateClassParameters state)
  })

getClassParameter :: QName -> M QName
getClassParameter className = do
  state <- getFS
  if M.member className (stateClassParameters state)
   then return $
        M.findWithDefault undefined className (stateClassParameters state)
   else error ("(Undeclared class \"" ++ show className ++ "\")")

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
mangleClassDataType className = Name ("class{" ++ show className ++ "}")

mangleClassConstructor :: QName -> QName
mangleClassConstructor className = Name ("mk{" ++ show className ++ "}")

mangleInstanceName :: QName -> QName -> QName
mangleInstanceName className typeName =
  Name ("instance{" ++ show className ++ "}{" ++ show typeName ++ "}")

---- Constraints

freshPlaceholder :: M PlaceholderId
freshPlaceholder = do 
  state <- getFS
  putFS (state {
    stateFreshPlaceholder = stateFreshPlaceholder state + 1
  })
  return $ stateFreshPlaceholder state

setMetavarPlaceholder :: QName -> TypeMetavariable -> PlaceholderId -> M ()
setMetavarPlaceholder className metavar plh = do
  modifyFS (\ state -> state {
    stateConstraintEnv =
      M.insert (className, metavar) plh (stateConstraintEnv state)
  })

getConstraints :: M (S.Set (QName, TypeMetavariable))
getConstraints = do
  state <- getFS
  return $ M.keysSet (stateConstraintEnv state)

getConstraintsForMetavar :: TypeMetavariable
                         -> M [(QName, PlaceholderId)]
getConstraintsForMetavar metavar = do
  state <- getFS
  return $
    concatMap (\ ((className, metavar'), plh) ->
                if metavar == metavar'
                  then [(className, plh)]
                  else [])
              (M.toList (stateConstraintEnv state))

getConstraintPlaceholder :: QName -> TypeMetavariable
                         -> M (Maybe PlaceholderId)
getConstraintPlaceholder className metavar = do
  state <- getFS
  return $ M.lookup (className, metavar) (stateConstraintEnv state)

instantiatePlaceholder :: PlaceholderId -> Expr -> M ()
instantiatePlaceholder plh expr = do
  modifyFS (\ state -> state {
    statePlaceholderHeap = M.insert plh expr (statePlaceholderHeap state)
  })

unfoldProgramPlaceholdersM :: Program -> M Program
unfoldProgramPlaceholdersM program = do
  state <- getFS
  case unfoldPlaceholders (statePlaceholderHeap state) program of
    Right program' -> return program'
    Left  expr     -> do
      setPosition (exprAnnotation expr)
      failM ConstraintErrorUnresolvedPlaceholder
            ("Unresolved instance constraint.\n" ++
             "Pending instance placeholder: " ++ show expr)

addGlobalInstance :: QName -> Type -> [(QName, QName)] -> M ()
addGlobalInstance className typ constraints = do
  state <- getFS
  case typeHead typ of
    TVar typeConstructor ->
      let typeParameters = map unTVar (typeArgs typ) in
        if M.member (className, typeConstructor) (stateGlobalInstances state)
         then failM InstanceErrorDuplicateInstance
                    ("Instance \"" ++ show className ++ "\"" ++
                     " for \"" ++ show typeConstructor ++ "\"" ++
                     " already declared.")
         else putFS (state {
                stateGlobalInstances   =
                  M.insert (className, typeConstructor)
                           (GlobalInstance className
                                           typeConstructor
                                           typeParameters
                                           constraints)
                           (stateGlobalInstances state)
              })
    _ -> error "(Head of instance declaration must be a type variable)"

addGlobalTypeConstraint :: TypeConstraint -> M () 
addGlobalTypeConstraint (TypeConstraint c t) = addGlobalInstance c t []

addGlobalConstraint :: Constraint -> M () 
addGlobalConstraint (Constraint _ c t) = addGlobalInstance c (TVar t) []

removeGlobalInstance  :: QName -> Type -> M ()
removeGlobalInstance className typ = do
  state <- getFS
  case typeHead typ of
    TVar typeConstructor ->
      putFS (state {
        stateGlobalInstances =
          M.delete (className, typeConstructor) (stateGlobalInstances state)
      })
    _ -> error "(Head of instance declaration must be a type variable)"

removeGlobalTypeConstraint :: TypeConstraint -> M () 
removeGlobalTypeConstraint (TypeConstraint c t) = removeGlobalInstance c t

removeGlobalConstraint :: Constraint -> M ()
removeGlobalConstraint (Constraint _ c t) = removeGlobalInstance c (TVar t)

lookupGlobalInstance :: QName -> QName -> M GlobalInstance
lookupGlobalInstance className typeConstructor = do
    state <- getFS
    if M.member key (stateGlobalInstances state)
     then return $ M.findWithDefault undefined key (stateGlobalInstances state)
     else failM ConstraintErrorUndeclaredInstance
                ("Unsolvable constraint.\n" ++
                 "Undeclared instance \"" ++ show className ++ "\"" ++
                 " for \"" ++ show typeConstructor ++ "\".")
  where key = (className, typeConstructor)

-------------------------------------------------------------------------------

---- Type inference algorithm

inferTypeProgramM :: Program -> M Program
inferTypeProgramM (Program decls) = do
  -- Declare built-in type constructors
  addTypeConstant primitiveArrow
  addTypeConstant primitiveUnit
  addTypeConstant primitiveInt
  -- Declare types of built-in functions
  let tA = Name "{a}"
  let tB = Name "{b}"
  bindType primitiveAlternative
           (TypeScheme [tA] (ConstrainedType []
              (tFun (TVar tA) (tFun (TVar tA) (TVar tA)))))
  bindType primitiveSequence
           (TypeScheme [tA, tB] (ConstrainedType []
              (tFun (TVar tA) (tFun (TVar tB) (TVar tB)))))
  bindType primitiveUnification
           (TypeScheme [tA] (ConstrainedType []
              (tFun (TVar tA) (tFun (TVar tA) (TVar primitiveUnit)))))
  bindType primitiveUnit
           (TypeScheme [] (ConstrainedType [] (TVar primitiveUnit)))
  bindType primitivePrint
           (TypeScheme [tA] (ConstrainedType []
              (tFun (TVar tA) (TVar primitiveUnit))))
  bindType primitiveUnderscore
           (TypeScheme [tA] (ConstrainedType [] (TVar tA)))
  enterScopeM
  -- Infer
  mapM_ collectTypeDeclarationM decls
  mapM_ collectSignaturesM decls
  decls' <- inferTypeDeclarationsM decls
  unfoldProgramPlaceholdersM (Program decls')

collectTypeDeclarationM :: Declaration -> M ()
collectTypeDeclarationM (TypeDeclaration pos typ value) = do
  case exprHeadVariable typ of
    Just name -> addTypeConstant name
    _ -> error "(Type has no head variable)"
  let (name, args) = splitDatatypeArgsOrFail typ in
    defineTypeSynonym name args (exprToType value)
collectTypeDeclarationM (DataDeclaration _ typ _) = do
  case exprHeadVariable typ of
    Just name -> addTypeConstant name
    _ -> error "(Type has no head variable)"
collectTypeDeclarationM _ = return ()

collectSignaturesM :: Declaration -> M ()
collectSignaturesM (DataDeclaration pos typ constructors) = do
  mapM_ collectGenericSignatureM constructors
collectSignaturesM (TypeSignature signature) = collectSignatureM signature
collectSignaturesM (ClassDeclaration pos className typeName methods) = do
    setClassParameter className typeName
    mapM_ collectMethodSignature methods
  where
    collectMethodSignature :: Signature -> M ()
    collectMethodSignature sig@(Signature pos methodName typ cs) = do
      setPosition pos
      addClassMethodSignature className sig
      if (any (\(Constraint _ cClass ctype) -> typeName == ctype) cs)
       then failM ClassErrorConstrainedParameter
              ("Class parameter " ++ show typeName ++ " cannot be constrained")
       else -- Add class constraint
            let c = Constraint pos className typeName in
              collectGenericSignatureM (Signature pos methodName typ (c : cs))
collectSignaturesM (InstanceDeclaration pos cls typ constraints _) = do
  setPosition pos
  addGlobalInstance cls (exprToType typ) 
                    (map (\ (Constraint _ c v) -> (c, v))
                         constraints)
collectSignaturesM _ = return ()

collectGenericSignatureM :: Signature -> M ()
collectGenericSignatureM (Signature pos name typ constraints) = do 
  setPosition pos
  ct <- constrainedType constraints typ
  cs <- allTypeConstants
  let fvariables = S.toList $ constrainedTypeFreeVariables ct S.\\ cs
  bindType name (TypeScheme fvariables ct)

collectSignatureM :: Signature -> M ()
collectSignatureM sig@(Signature pos name typ constraints) = do 
  collectGenericSignatureM sig
  TypeScheme fvariables ct <- lookupType name
  newVariables <- mapM (const freshTypeVar) fvariables
  overrideType name (TypeScheme []
                      (substituteConstrainedType
                        (M.fromList (zip fvariables newVariables))
                        ct))

inferTypeDeclarationsM :: [Declaration] -> M [Declaration]
inferTypeDeclarationsM decls =
    let definedVars = S.unions (map declarationHeadVars decls)
     in do mapM_ bindToFreshTypeIfNotLocallyBound (S.toList definedVars)
           rec Nothing decls
  where
    declarationHeadVars (ValueDeclaration (Equation _ lhs _)) =
      S.fromList [fromJust (exprHeadVariable lhs)]
    declarationHeadVars (MutualDeclaration _ decls) =
      S.unions (map declarationHeadVars decls)
    declarationHeadVars _ = S.empty

    -- Infer a list of type declarations, generalizing the types
    rec :: Maybe QName -> [Declaration] -> M [Declaration]
    rec prev [] = do
      recGeneralize prev Nothing
      return []
    rec prev (decl@(ValueDeclaration equation) : decls) = do
      let name = fromJust . exprHeadVariable . equationLHS $ equation
      recGeneralize prev (Just name)
      decl'  <- inferTypeDeclarationM decl
      decls' <- rec (Just name) decls
      return (decl' ++ decls')
    rec prev (MutualDeclaration pos decls : moreDecls) = do
      setPosition pos
      -- TODO: collect signatures
      (names, decls') <- recMutual decls
      generalizeLocalBindings names
      moreDecls' <- rec Nothing moreDecls
      return (decls' ++ moreDecls')
    rec prev (decl : decls) = do
      recGeneralize prev Nothing
      decl'  <- inferTypeDeclarationM decl
      decls' <- rec prev decls
      return (decl' ++ decls')

    -- Infer a list of type declarations without generalizing the types
    recMutual :: [Declaration] -> M (S.Set QName, [Declaration])
    recMutual [] = return (S.empty, [])
    recMutual (decl@(ValueDeclaration equation) : decls) = do
      let name = fromJust . exprHeadVariable . equationLHS $ equation
      decl'           <- inferTypeDeclarationM decl
      (names, decls') <- recMutual decls
      return (S.insert name names, decl' ++ decls')
    recMutual (decl@(MutualDeclaration pos _) : decls) = do
      setPosition pos
      failM TypeErrorNestedMutualDeclarations
            "Nested mutual declarations"
    recMutual (decl : decls) = do
      decl'           <- inferTypeDeclarationM decl
      (names, decls') <- recMutual decls
      return (names, decl' ++ decls')

    recGeneralize :: Maybe QName -> Maybe QName -> M ()
    recGeneralize Nothing     _       = return ()
    recGeneralize (Just name) Nothing = generalizeName name
    recGeneralize (Just name) (Just name')
      | name == name' = return ()
      | otherwise     = generalizeName name

    generalizeName :: QName -> M ()
    generalizeName name = generalizeLocalBindings (S.singleton name)

inferTypeDeclarationM :: Declaration -> M [Declaration]
inferTypeDeclarationM decl@(DataDeclaration _ _ _) = return [decl]
inferTypeDeclarationM decl@(TypeDeclaration _ _ _) = return [decl]
inferTypeDeclarationM (ValueDeclaration equation) = do
  equation' <- inferTypeEquationM equation
  return [ValueDeclaration equation']
inferTypeDeclarationM decl@(TypeSignature _) = do
  return [decl]
inferTypeDeclarationM (ClassDeclaration pos className typeName methods) = do
  inferTypeClassDeclarationM pos className typeName methods
inferTypeDeclarationM (InstanceDeclaration pos className typ
                                           constraints methods) = do
  decl <- inferTypeInstanceDeclarationM pos className typ constraints methods
  return [decl]
inferTypeDeclarationM (MutualDeclaration _ _) =
  error "(Mutual declaration should not be found)"

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
      lfunc   = fromJust $ exprHeadVariable lhs
      largs   = fromJust $ exprHeadArguments lhs in do
    -----------
    enterScopeM
    mapM_ bindToFreshType lhsFree
    (TypeScheme names (ConstrainedType lfuncConstraints lfuncType)) <-
      lookupType lfunc
    if not (null names)
      then error ("(Local variables "
                  ++ show names
                  ++ " may not be generalized)")
      else return ()
    mapM_ addGlobalTypeConstraint lfuncConstraints
    (largsConstraints, largsTypes, largs') <-
      splitResults <$> mapM inferTypeExprM largs
    (ConstrainedType rConstraints rType, rhs') <- inferTypeExprM rhs
    unifyTypes lfuncType (foldr tFun rType largsTypes)
    mapM_ removeGlobalTypeConstraint lfuncConstraints
    exitScopeM
    -----------
    let lfunc' = foldl (EApp pos) (EVar pos lfunc)
                       (map constraintsParameterName lfuncConstraints)
    return $ Equation pos (foldl (EApp pos) lfunc' largs') rhs'
  where
    constraintsParameterName :: TypeConstraint -> Expr
    constraintsParameterName (TypeConstraint className (TVar name)) =
      EVar pos $ mangleInstanceName className name
    constraintsParameterName (TypeConstraint className _) =
      error "(Function constraint must be a variable)"

    splitResults :: [InferResult] -> ([TypeConstraint], [Type], [Expr])
    splitResults [] = ([], [], [])
    splitResults ((ConstrainedType cs typ, expr) : results) =
      let (css, typs, exprs) = splitResults results
        in (union cs css, typ : typs, expr : exprs)

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

-- Identify all the type variables and type metavariables in the local
-- environment that can be generalized with a forall. Update all the
-- bindings in the local environment so that all these type variables
-- become generalized.
generalizeLocalBindings :: S.Set QName -> M ()
generalizeLocalBindings affectedNames = do
    (genMetavars, genVars) <- generalizableVariables
    rib <- head <$> getEnvironment
    mapM_ (uncurry $ generalizeLocalBinding genMetavars genVars) (M.toList rib)
  where
    generalizableVariables :: M (S.Set TypeMetavariable, S.Set QName)
    generalizableVariables = do
      rib  <- head <$> getEnvironment
      ribs <- tail <$> getEnvironment
      -- Generalizable type variables
      localVars <- getVars rib
      fixedVars <- S.unions <$> mapM getVars ribs
      constantVars <- allTypeConstants
      let genVars = S.difference localVars (fixedVars `S.union` constantVars)
      -- Generalizable type metavariables
      localMetavars <- getMetavars rib
      fixedMetavars <- S.unions <$> mapM getMetavars ribs
      let genMetavars  = S.difference localMetavars fixedMetavars
      let genMetavarsL = S.toList genMetavars
      genVars' <- mapM (const freshTypeVar) genMetavarsL
      mapM_ (uncurry setRepresentative) (zip genMetavarsL genVars')
      --
      return (genMetavars, genVars `S.union` S.fromList (map unTVar genVars'))
    getVars :: M.Map QName TypeScheme -> M (S.Set QName)
    getVars rib = do
      schemes <- concat <$> mapM (lookupInRib rib) (S.toList affectedNames)
      return $ S.unions (map typeSchemeFreeVariables schemes)
    getMetavars :: M.Map QName TypeScheme -> M (S.Set TypeMetavariable)
    getMetavars rib = do
      schemes <- concat <$> mapM (lookupInRib rib) (S.toList affectedNames)
      return $ S.unions (map typeSchemeMetavariables schemes)
    lookupInRib :: M.Map QName TypeScheme -> QName -> M [TypeScheme]
    lookupInRib rib name = do
      case M.lookup name rib of
        Nothing -> return []
        Just ts -> do ts' <- unfoldTypeScheme ts
                      return [ts']
    generalizeLocalBinding :: S.Set TypeMetavariable -> S.Set QName
                           -> QName -> TypeScheme -> M ()
    generalizeLocalBinding genMetavars genVars x scheme = do
      scheme'@(TypeScheme vars (ConstrainedType constraints typ)) <-
              unfoldTypeScheme scheme
      constraints' <- constraintsFor genMetavars
      let genVars' = S.toList (S.intersection
                                genVars
                                (typeSchemeFreeVariables scheme'))
       in overrideType x (TypeScheme (genVars' ++ vars)
                            (ConstrainedType (constraints' ++ constraints) typ))
    constraintsFor :: S.Set TypeMetavariable -> M [TypeConstraint]
    constraintsFor genMetavars = do
      cs <- getConstraints
      let cs' = filter (\ (_, metavar) -> S.member metavar genMetavars)
                       (S.toList cs)
      mapM (\ (className, metavar) -> do
             typ <- unfoldType (TMetavar metavar)
             return $ TypeConstraint className typ)
           cs'

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
       else instantiateAndSolveConstraints x t 
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

    instantiateAndSolveConstraints :: TypeMetavariable -> Type -> M ()
    instantiateAndSolveConstraints x t0 = do
      t <- unfoldType t0
      setRepresentative x t
      cs <- getConstraintsForMetavar x
      mapM_ (\ (cls, plh) -> solveConstraint cls plh t) cs

    solveConstraint :: QName -> PlaceholderId -> Type -> M ()
    solveConstraint className plh (TMetavar metavar) = do
      mq  <- getConstraintPlaceholder className metavar
      pos <- currentPosition
      case mq of
        Just q  -> instantiatePlaceholder plh (EPlaceholder pos q)
        Nothing -> setMetavarPlaceholder className metavar plh
    solveConstraint className plh typ =
      case typeHead typ of
        TVar typeConstructor -> do
          pos <- currentPosition
          inst <- lookupGlobalInstance className typeConstructor
          -- Create placeholders plh1, ..., plhN, one per subgoal
          plhs <- mapM (const freshPlaceholder)
                       (globalInstanceConstraints inst)
          -- Instantiate the main placeholder in
          --   instance{className}{typeConstructor} plh1 ... plhN
          instantiatePlaceholder plh
              (foldl (EApp pos)
                     (EVar pos (mangleInstanceName className typeConstructor))
                     (map (EPlaceholder pos) plhs))
          let typeArguments = typeArgs typ
          let paramDict = M.fromList
                            (zip (globalInstanceTypeParameters inst)
                                 typeArguments)
          -- Recursively solve subgoals
          mapM_ (\ ((cls, param), plh) ->
                  solveConstraint cls plh
                    (M.findWithDefault undefined param paramDict)) 
                (zip (globalInstanceConstraints inst) plhs)
          return ()
        _ -> error "(Head of constrained type must be a type variable)"

type InferResult = (ConstrainedType, Expr)

inferTypeExprM :: Expr -> M InferResult
inferTypeExprM (EInt pos n)               = inferTypeEIntM pos n
inferTypeExprM (EVar pos x)               = inferTypeEVarM EVar pos x
inferTypeExprM (EUnboundVar pos x)        = inferTypeEVarM EUnboundVar pos x
inferTypeExprM (EApp pos e1 e2)           = inferTypeEAppM pos e1 e2
inferTypeExprM (ELambda pos e1 e2)        = inferTypeELambdaM pos e1 e2
inferTypeExprM (ELet pos decls body)      = inferTypeELetM pos decls body
inferTypeExprM (ECase pos guard branches) = inferTypeECaseM pos guard branches
inferTypeExprM (EFresh pos x body)        = inferTypeEFreshM pos x body
inferTypeExprM (EPlaceholder _ _)         =
  error "(Impossible: type inference of an instance placeholder)"

-- Integer constant (n)
inferTypeEIntM :: Position -> Integer -> M InferResult
inferTypeEIntM pos n = return (ConstrainedType [] tInt, EInt pos n)

-- Variable (x)
inferTypeEVarM :: (Position -> QName -> Expr)
               -> Position -> QName -> M InferResult
inferTypeEVarM eVar pos x = do
  setPosition pos
  -- Lookup the type scheme.
  TypeScheme gvars constrainedType   <- lookupType x
  -- Instantiate all general variables into fresh metavariables.
  -- Each general variable affected by a class constraint
  -- produces a new placeholder for the instance.
  (plhs, constrainedType') <- freshenVariables gvars constrainedType
  -- Return the variable applied to all the placeholders.
  return (constrainedType',
          foldl (\ e p -> EApp pos e (EPlaceholder pos p))
                (eVar pos x)
                plhs)

-- e1 e2
inferTypeEAppM :: Position -> Expr -> Expr -> M InferResult
inferTypeEAppM pos e1 e2 = do
  (ConstrainedType c1 t1, e1') <- inferTypeExprM e1
  (ConstrainedType c2 t2, e2') <- inferTypeExprM e2
  tr <- freshType
  setPosition pos
  unifyTypes t1 (tFun t2 tr)
  return (ConstrainedType (union c1 c2) tr, (EApp pos e1' e2'))

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
  enterScopeM
  mapM_ collectSignaturesM decls
  decls' <- inferTypeDeclarationsM decls
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
  return (ConstrainedType (cguard ++ cbranches) tresult,
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
                              M [Declaration]
inferTypeClassDeclarationM pos className typeName methods = do
    setPosition pos
    constrainedTypes <- mapM renameMethodSignature methods
    methodDecls      <- buildMethodDeclarations
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
      in return (DataDeclaration pos classDatatype' [constructorSig]
                 : methodDecls)
  where
    -- Rename all the type variables in the method signature to fresh names,
    -- except for the class parameter (and constants).
    renameMethodSignature :: Signature -> M ConstrainedType
    renameMethodSignature (Signature pos methodName typ cs) = do
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
    
    buildMethodDeclarations :: M [Declaration]
    buildMethodDeclarations = do
      pos <- currentPosition
      classMethodNames <- getClassMethodNames className
      let n = length classMethodNames
      let params = map (\ i -> EUnboundVar pos (Name ("x{" ++ show i ++ "}")))
                        [1..n]
      return $ map
        (\ (methodName, index) -> ValueDeclaration
          (Equation pos
            (EApp pos (EVar pos methodName) (EUnboundVar pos (Name "{inst}")))
            (ECase pos (EVar pos (Name "{inst}")) [
              CaseBranch pos
                (foldl (EApp pos)
                  (EVar pos (mangleClassConstructor className))
                  params)
                (EVar pos (Name ("x{" ++ show index ++ "}")))
            ])))
        (zip classMethodNames [1..n])

inferTypeInstanceDeclarationM :: Position -> QName -> Expr
                                 -> [Constraint] -> [Equation]
                                 -> M Declaration
inferTypeInstanceDeclarationM pos className typ constraints methodEqs = do
    setPosition pos
    instanceDecl <- buildInstanceRecordDeclaration
    return instanceDecl
  where
    (typeConstructor, typeParams) = splitDatatypeArgsOrFail typ
    buildInstanceRecordDeclaration =
      let lhsHead = EUnboundVar pos $
                    mangleInstanceName className typeConstructor
          instanceConstraints = map (\ (Constraint pos c t) ->
                                        EUnboundVar pos $
                                        mangleInstanceName c t)
                                    constraints
          lhs = foldl (EApp pos) lhsHead instanceConstraints
       in do
         enterScopeM
         mapM_ addGlobalConstraint constraints
         methodEqs' <- mapM
                         (inferTypeMethodEquationM className typ constraints)
                         methodEqs
         mapM_ removeGlobalConstraint constraints
         exitScopeM
         rhs <- buildRHS methodEqs'
         return $ ValueDeclaration (Equation pos lhs rhs)
    buildRHS methodEqs' = do
      classMethodNames <- getClassMethodNames className
      case groupEquations methodEqs' of
        Left  errmsg -> failM InstanceErrorDuplicatedMethodDefinition errmsg
        Right methodEqs'' -> do
          let instanceMethodNames  = map (unEVar . equationLHS) methodEqs''
          let classMethodNamesS    = S.fromList classMethodNames
          let instanceMethodNamesS = S.fromList instanceMethodNames
          if classMethodNamesS /= instanceMethodNamesS
           then failM InstanceErrorMethodMismatch
                      (methodMismatchMessage classMethodNamesS
                                             instanceMethodNamesS)
           else do
             -- Create a fresh name for each method in the instance structure 
             localMethodNames <- mapM (const freshEVar) classMethodNames
             let methodDictionary = M.fromList (map unEquation methodEqs'')
             let localMethods = [
                    Equation pos (varToUnboundVar lm)
                      (M.findWithDefault undefined cm methodDictionary)
                    | (cm, lm) <- zip classMethodNames localMethodNames ]
             return $ ELet pos
                        (map ValueDeclaration localMethods)
                        (foldl (EApp pos)
                               (EVar pos (mangleClassConstructor className))
                               localMethodNames)
    varToUnboundVar :: Expr -> Expr
    varToUnboundVar (EVar pos var) = EUnboundVar pos var
    varToUnboundVar _              = error "(Must be a variable)"
    methodMismatchMessage cs is =
      "Instance methods do not match class methods."
       ++ (if cs `S.isSubsetOf` is
            then ""
            else "\nNot in instance: " ++
                  unwords (map show (S.toList (cs S.\\ is))))
       ++ (if is `S.isSubsetOf` cs
            then ""
            else "\nNot in class: " ++
                  unwords (map show (S.toList (is S.\\ cs))))
    unEquation :: Equation -> (QName, Expr)
    unEquation eq = (unEVar (equationLHS eq), equationRHS eq)

inferTypeMethodEquationM :: QName -> Expr -> [Constraint] -> Equation
                         -> M Equation
inferTypeMethodEquationM className instantiatedType instantiatedTypeConstraints
                         (Equation pos lhs rhs) = do
  setPosition pos
  bound <- getAllBoundVars
  let lhsFree = exprFreeVariables bound lhs
      methodName = fromJust $ exprHeadVariable lhs
      arguments  = fromJust $ exprHeadArguments lhs
   in do
     classParameter <- getClassParameter className
     (instantiatedConstraintMap, ConstrainedType sigConstraints sigType) <-
       classMethodFreshType className methodName
                            classParameter
                            instantiatedType instantiatedTypeConstraints
     mapM_ addGlobalTypeConstraint sigConstraints
     mapM_ addGlobalTypeConstraint (map snd instantiatedConstraintMap)
     enterScopeM
     mapM_ bindToFreshType lhsFree
     argResults <- mapM inferTypeExprM arguments
     let argTypes       = map (ctType . fst) argResults
     let argConstraints = map (ctConstraints . fst) argResults
     let arguments'     = map snd argResults
     (ConstrainedType rhsConstraints tRHS, rhs') <- inferTypeExprM rhs
     setPosition pos
     unifyTypes sigType (foldr tFun tRHS argTypes)
     exitScopeM
     mapM_ removeGlobalTypeConstraint (map snd instantiatedConstraintMap)
     mapM_ removeGlobalTypeConstraint sigConstraints
     let lhs' =
          foldl (EApp pos) (EVar pos methodName)
                (map (\ (TypeConstraint cls typ) ->
                        EUnboundVar pos (mangleInstanceName cls (unTVar typ)))
                     sigConstraints
                 ++ arguments')
     let instantiatedConstraintDecls = 
           [ValueDeclaration
             (Equation pos
               (EUnboundVar pos (mangleInstanceName cls2 (unTVar typ2)))
               (EVar pos (mangleInstanceName cls1 typ1)))
           | (Constraint _ cls1 typ1, TypeConstraint cls2 typ2) <-
             instantiatedConstraintMap
           ]
     return $ Equation pos lhs' (ELet pos instantiatedConstraintDecls rhs')
  where
    ctType        (ConstrainedType _ t) = t
    ctConstraints (ConstrainedType c _) = c

classMethodFreshType :: QName -> QName -> QName -> Expr -> [Constraint]
                              -> M ([(Constraint, TypeConstraint)],
                                    ConstrainedType)
classMethodFreshType className methodName classParameter
                     instantiatedType instantiatedTypeConstraints = do
  methodSignature <- getClassMethodSignature className methodName
  let sigType        = exprToType (signatureType methodSignature)
  let sigConstraints = map constraintToTypeConstraint
                         (signatureConstraints methodSignature)
  let sigConstrainedType = ConstrainedType sigConstraints sigType
  constants <- allTypeConstants
  let fvariables = S.toList
                     (constrainedTypeFreeVariables sigConstrainedType
                      S.\\ (constants `S.union` S.singleton classParameter))
  newVariables <- mapM (const freshTypeVar) fvariables
  (ConstrainedType instantiatedTypeConstraints' instantiatedType') <-
    refreshConstrainedType instantiatedType instantiatedTypeConstraints
  let substitution = M.fromList ((classParameter, instantiatedType')
                                : zip fvariables newVariables)
  let (ConstrainedType sigConstraints' sigType') = substituteConstrainedType
                                                     substitution
                                                     sigConstrainedType
  return $ (zip instantiatedTypeConstraints instantiatedTypeConstraints',
            ConstrainedType sigConstraints' sigType')

refreshConstrainedType :: Expr -> [Constraint] -> M ConstrainedType
refreshConstrainedType typ constraints = do
  let constrainedType = ConstrainedType
                          (map constraintToTypeConstraint constraints)
                          (exprToType typ)
  constants <- allTypeConstants
  let fvariables = S.toList (constrainedTypeFreeVariables constrainedType
                            S.\\ constants)
  newVariables <- mapM (const freshTypeVar) fvariables
  let substitution = M.fromList (zip fvariables newVariables)
  return $ substituteConstrainedType substitution constrainedType

unions :: Eq a => [[a]] -> [a]
unions = foldr union []

