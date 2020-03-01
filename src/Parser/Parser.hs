module Parser.Parser(parse) where

import Debug.Trace

import qualified Data.Set as S
import Data.List(isPrefixOf)

import Error(Error(..), ErrorType(..), ErrorMessage)
import Position(Position(..), unknownPosition)
import FailState(FailState, getFS, modifyFS, putFS, evalFS, failFS)
import Syntax.Name(QName(..), readName, qualify, moduleNameFromQName,
                   unqualifiedName, splitParts, allNameParts)
import Syntax.AST(
         Program(..), AnnDeclaration(..), Declaration, AnnExpr(..), Expr,
         exprIsVariable, exprHeadVariable
       )
import Lexer.Token(Token(..), TokenType(..))

import Parser.PrecedenceTable(
         PrecedenceTable(..), Associativity(..),
         PrecedenceLevel, Precedence, precedenceTableLevels,
         emptyPrecedenceTable, declareOperator, isOperatorPart,
         operatorFixity
       )

import Parser.ModuleSystem.Module(
         Module,
           emptyModule, addSubmodule, exportAllNamesFromModule, exportNames,
           declareName
       )
import Parser.ModuleSystem.Context(
         Context,
           emptyContext, contextCurrentModuleName,
           resolveName, importAllNamesFromModule, importNames
       )

parse :: [Token] -> Either Error Program
parse tokens = evalFS parseM initialState
  where initialState =
          ParserState {
            stateInput           = tokens,
            statePosition        = tokensPosition tokens unknownPosition,
            stateRootModule      = emptyModule,
            stateNameContext     = error "Empty name context.",
            statePrecedenceTable = emptyPrecedenceTable
          }

---- Parser monad

data ParserState = ParserState {
                     stateInput           :: [Token],
                     statePosition        :: Position,
                     stateRootModule      :: Module,
                     stateNameContext     :: Context,
                     statePrecedenceTable :: PrecedenceTable
                   }

type M = FailState ParserState

tokensPosition :: [Token] -> Position -> Position
tokensPosition tokens elsePos =
  if null tokens
   then elsePos
   else tokenStartPos (head tokens)

peek :: M Token
peek = do
  state <- getFS
  pos   <- currentPosition
  if null (stateInput state)
   then return $ Token pos pos T_EOF
   else return $ head (stateInput state) 

peekType :: M TokenType
peekType = do
  tok <- peek
  return $ tokenType tok

peekIsRParen :: M Bool
peekIsRParen = do
  t <- peekType
  case t of
    T_RParen -> return True
    _        -> return False

getToken :: M Token
getToken = do
  state <- getFS
  pos   <- currentPosition
  if null (stateInput state)
   then return $ Token pos pos T_EOF
   else let (tok : toks) = stateInput state in do
          putFS (state {
                   stateInput    = toks,
                   statePosition = tokensPosition toks pos
                 })
          return tok

match :: TokenType -> M ()
match t = do
  t' <- peekType
  if t == t'
   then do getToken
           return ()
   else failM ParseError
              ("Expected: " ++ show t ++ ".\n" ++
               "Got     : " ++ show t' ++ ".")

currentPosition :: M Position
currentPosition = do
  state <- getFS
  return $ statePosition state

failM :: ErrorType -> String -> M a
failM errorType msg = do
  pos <- currentPosition
  failFS (Error errorType pos msg)

getRootModule :: M Module
getRootModule = do
  state <- getFS
  return $ stateRootModule state

getNameContext :: M Context
getNameContext = do
  state <- getFS
  return $ stateNameContext state

getPrecedenceTable :: M PrecedenceTable
getPrecedenceTable = do
  state <- getFS
  return $ statePrecedenceTable state

getCurrentModuleName :: M QName
getCurrentModuleName = do
  nameContext <- getNameContext
  return $ contextCurrentModuleName nameContext

modifyRootModule :: (Module -> Either ErrorMessage Module) -> M ()
modifyRootModule f = do
  state <- getFS
  case f (stateRootModule state) of
    Left errmsg ->
      failM ModuleSystemError errmsg
    Right stateRootModule' ->
      putFS (state { stateRootModule = stateRootModule' })

modifyNameContext :: (Context -> Either ErrorMessage Context) -> M ()
modifyNameContext f = do
  state <- getFS
  case f (stateNameContext state) of
    Left errmsg ->
      failM ModuleSystemError errmsg
    Right stateNameContext' ->
      putFS (state { stateNameContext = stateNameContext' })

modifyPrecedenceTable ::
  (PrecedenceTable -> Either ErrorMessage PrecedenceTable) -> M ()
modifyPrecedenceTable f = do
  state <- getFS
  case f (statePrecedenceTable state) of
    Left errmsg ->
      failM ModuleSystemError errmsg
    Right precedenceTable' ->
      putFS (state { statePrecedenceTable = precedenceTable' })

enterModule :: QName -> M ()
enterModule moduleName = do
  state <- getFS
  case addSubmodule moduleName (stateRootModule state) of
    Left  errmsg ->
      failM ModuleSystemError errmsg
    Right stateRootModule' ->
      putFS (state {
        stateRootModule  = stateRootModule',
        stateNameContext = emptyContext moduleName
      })

exportAllNamesFromModuleM :: QName -> M ()
exportAllNamesFromModuleM moduleName =
  modifyRootModule (exportAllNamesFromModule moduleName)

exportNamesM :: QName -> [String] -> M ()
exportNamesM moduleName exportedNames =
  modifyRootModule (exportNames moduleName exportedNames)

declareQNameM :: QName -> M ()
declareQNameM qname = do
  moduleName <- getCurrentModuleName
  let uname = unqualifiedName qname in
    if qname == qualify moduleName uname
     then modifyRootModule (declareName qname)
     else failM ModuleSystemError
                ("Declaration of name \"" ++ uname ++ "\"" ++
                 " shadows \"" ++ show qname ++ "\".")

importAllNamesFromModuleM :: QName -> M ()
importAllNamesFromModuleM moduleName = do
  rootModule <- getRootModule
  modifyNameContext (importAllNamesFromModule moduleName rootModule)

importNamesM :: QName -> [(String, String)] -> M ()
importNamesM moduleName renamings = do
  rootModule <- getRootModule
  modifyNameContext (importNames moduleName renamings rootModule)

declareOperatorM :: Associativity -> Precedence -> QName -> M ()
declareOperatorM assoc precedence qname = do
  declareQNameM qname
  modifyPrecedenceTable (declareOperator assoc precedence qname)

isOperatorPartM :: QName -> M Bool
isOperatorPartM qname = do
  table <- getPrecedenceTable
  return $ isOperatorPart qname table

isLeftAssocM :: QName -> M Bool
isLeftAssocM op = do
  table <- getPrecedenceTable
  case operatorFixity op table of
    LeftAssoc -> return True
    _         -> return False

isRightAssoc :: QName -> PrecedenceTable -> Bool
isRightAssoc op table =
  case operatorFixity op table of
    RightAssoc -> True
    _          -> False

---- Parser

parseM :: M Program
parseM = do
  decls <- parseModules 
  return $ Program {
             programDeclarations = decls
           }

parseModules :: M [Declaration]
parseModules = do
  t <- peekType
  case t of
    T_Module -> do ds1 <- parseModule
                   ds2 <- parseModules
                   return (ds1 ++ ds2)
    T_LBrace -> do ds1 <- parseBareModule
                   ds2 <- parseModules
                   return (ds1 ++ ds2)
    T_EOF    -> return []
    _        -> failM ParseError
                      ("Expected a module, but got: " ++ show t ++ ".")

parseModule :: M [Declaration]
parseModule = do
  match T_Module
  qname <- parseQName
  enterModule qname
  parseModuleExports qname
  match T_Where
  match T_LBrace
  decls <- parseDeclarations
  match T_RBrace
  return decls

parseBareModule :: M [Declaration]
parseBareModule = do
  enterModule (Name "Main") -- Export all names
  match T_LBrace
  decls <- parseDeclarations
  match T_RBrace
  return decls

parseSequence :: M Bool -> M () -> M a -> M [a]
parseSequence checkTermination matchSeparator parseElement = do
    b <- checkTermination
    if b
     then return []
     else rec
  where
    rec = do
      elem  <- parseElement
      b     <- checkTermination
      if b
       then return [elem]
       else do matchSeparator
               elems <- rec
               return (elem : elems)

parseModuleExports :: QName -> M ()
parseModuleExports moduleName = do
  t <- peekType
  case t of
    T_LParen -> do
      match T_LParen
      exportedNames <- parseSequence peekIsRParen (match T_Semicolon) parseId 
      match T_RParen
      exportNamesM moduleName exportedNames
    _ ->
      exportAllNamesFromModuleM moduleName

parseQName :: M QName
parseQName = do
  id  <- parseId
  t <- peekType
  case t of
    T_Dot -> do
      match T_Dot
      qname <- parseQName
      return $ Qualified id qname
    _ ->
      return $ readName id

parseAndResolveQName :: M QName
parseAndResolveQName = do
  qname <- parseQName
  state <- getFS
  case resolveName (stateRootModule state) (stateNameContext state) qname of
    Left  errmsg -> failM ModuleSystemError errmsg
    Right qname' -> return qname'

peekAndResolveQName :: M (Maybe QName)
peekAndResolveQName = do
  t <- peekType
  case t of
    T_Id _ -> do state <- getFS
                 qname <- parseAndResolveQName
                 putFS state
                 return $ Just qname
    _ -> return Nothing

parseId :: M String
parseId = do
  t <- peekType
  case t of
    T_Id id -> do getToken
                  return id
    _       -> failM ParseError
                     ("Expected an identifier.\n" ++
                      "Got: " ++ show t ++ ".")

parseInt :: M Integer
parseInt = do
  t <- peekType
  case t of
    T_Int n -> do getToken
                  return n
    _       -> failM ParseError
                     ("Expected an integer.\n" ++
                      "Got: " ++ show t ++ ".")

parseDeclarations :: M [Declaration]
parseDeclarations = do
  t <- peekType
  case t of
    T_RBrace -> return []
    _        -> do
      ds1 <- parseDeclaration
      t'  <- peekType
      case t' of
        T_Semicolon -> do
          match T_Semicolon
          ds2 <- parseDeclarations
          return (ds1 ++ ds2)
        T_RBrace    -> return ds1
        _           -> failM ParseError
                             ("Expected \";\" or \"}\".\n" ++
                              "Got: " ++ show t' ++ ".")

parseDeclaration :: M [Declaration]
parseDeclaration = do
  t <- peekType
  case t of
    T_Import -> do parseImport
                   return []
    T_Infix  -> do parseFixityDeclaration NonAssoc
                   return []
    T_Infixl -> do parseFixityDeclaration LeftAssoc
                   return []
    T_Infixr -> do parseFixityDeclaration RightAssoc
                   return []
    T_Data   -> do d <- parseDataDeclaration
                   return [d]
    -- TODO: other kinds of declarations
    T_Id _   -> do d <- parseTypeSignatureOrValueDeclaration
                   return [d]
    _        -> failM ParseError
                       ("Expected a declaration.\n" ++
                        "Got: " ++ show t ++ ".")

parseImport :: M ()
parseImport = do
  match T_Import
  moduleName <- parseQName
  t <- peekType
  case t of
    T_LParen -> do match T_LParen
                   renamings  <- parseSequence peekIsRParen
                                               (match T_Semicolon)
                                               parseRenameId
                   importNamesM moduleName renamings
                   match T_RParen
    _ -> importAllNamesFromModuleM moduleName

parseRenameId :: M (String, String)
parseRenameId = do
  id <- parseId
  t <- peekType
  case t of
    T_As -> do match T_As
               alias <- parseId
               return (id, alias)
    _ -> return (id, id)

parseDataDeclaration :: M Declaration
parseDataDeclaration = do
  pos <- currentPosition
  expr <- parseExpr
  return $ dataDeclaration pos expr

dataDeclaration :: Position -> Expr -> M Declaration
dataDeclaration = pos (EVar _ name)  do
  match T_Where
  match T_LBrace
  constructors <- parseDataConstructorsM
  match T_RBrace
  return $ DataDeclaration pos name constructors

dataDeclaration _ _ =
  error "Expression leading data declaration must be a variable."

parseDataConstructorsM :: M [Expr]
parseDataConstructorsM = do
  constructor <- parseTypeSignature
  t <- peekType
  case t  of
    T_Semicolon -> do
                  getToken
                  constructors <- parseDataConstructorsM
                  return (constructor : constructors)
    _           -> do return [constructor]

parseFixityDeclaration :: Associativity -> M ()
parseFixityDeclaration assoc = do
  getToken
  precedence    <- parseInt
  operatorName  <- parseId
  currentModule <- getCurrentModuleName 
  declareOperatorM assoc precedence (qualify currentModule operatorName)

parseTypeSignatureOrValueDeclaration :: M Declaration
parseTypeSignatureOrValueDeclaration = do
  pos <- currentPosition
  expr <- parseExpr
  declareQNameM (exprHeadVariable expr)
  t <- peekType
  case t of
    T_Colon | exprIsVariable expr -> parseTypeSignature pos expr
    _ -> parseValueDeclaration pos expr

parseTypeSignature :: Position -> Expr -> M Declaration
parseTypeSignature pos (EVar _ name) = do
  match T_Colon
  typ <- parseExpr
  return $ TypeSignature pos name typ
parseTypeSignature _ _ =
  error "Expression leading type declaration must be a variable."

parseValueDeclaration :: Position -> Expr -> M Declaration
parseValueDeclaration pos lhs = do
  match T_Eq
  rhs <- parseExpr
  return $ ValueDeclaration pos lhs rhs

parseExpr :: M Expr
parseExpr = do
  table <- getPrecedenceTable
  parseExprMixfix (precedenceTableLevels table) table

{-- Mixfix parser --}

data Status = EmptyStatus Position
            | PushArgument Status Expr
            | PushOperatorPart Status QName

instance Show Status where
  show (EmptyStatus _) = ""
  show (PushOperatorPart status p) = show status ++ show p
  show (PushArgument status x)     = show status ++ "(" ++ show x ++ ")"

parseExprMixfix :: [PrecedenceLevel] -> PrecedenceTable -> M Expr
parseExprMixfix []     _     = parseApplication
parseExprMixfix levels table = do
  pos <- currentPosition
  parseExprInfix levels table (EmptyStatus pos)

parseExprInfix :: [PrecedenceLevel] -> PrecedenceTable -> Status -> M Expr
parseExprInfix []                      _     _      =
  error "parseExprInfix: list of levels cannot be empty"
parseExprInfix (currentLevel : levels) table status = do
   b <- isEndOfExpression
   if b
    then prematureEndOfExpression status
    else do
      b <- mustReadPart currentLevel table status
      if b
       then do
         part <- peekAndResolveQName >>= fromJustOrFail
         isOp <- isOperatorPartM part
         if isOp && statusIsValidPrefixInLevel
                        (PushOperatorPart status part)
                        currentLevel
          then do parseAndResolveQName
                  rec (PushOperatorPart status part)
          else prematureEndOfExpression status
       else do arg <- parseExprMixfix levels table
               rec (PushArgument status arg)
  where
    -- If the does not continue with an operator, there are two cases:
    --   If there is exactly one pushed argument, return it.
    --   Otherwise, fail.
    prematureEndOfExpression (PushArgument (EmptyStatus _) x) = return x
    prematureEndOfExpression _ =
      do t <- peekType
         failM ParseError
           ("Premature end of expression. Possibly expected operator part.\n" ++
            "Got: " ++ show t ++ ".\n" ++
            "Status: " ++ show status)

    -- Check whether we have to read an operator part.
    mustReadPart :: PrecedenceLevel -> PrecedenceTable -> Status -> M Bool
    mustReadPart level table status@(EmptyStatus _) = do
      maybeQName <- peekAndResolveQName
      case maybeQName of
        Nothing    -> return False
        Just qname -> do
          isOp <- isOperatorPartM qname
          return $ isOp && statusIsValidPrefixInLevel
                             (PushOperatorPart status qname)
                             level
    mustReadPart _ _ (PushArgument _ _)     = return True
    mustReadPart _ _ (PushOperatorPart _ _) = return False

    -- Continue reading the expression recursively.
    -- If we are finished, return.
    rec status'
      -- Case 1: "Almost finished" right associative operator
      --         (exactly one argument pending)
      | statusIsAlmostCompleteRightOperator status' currentLevel
        = do -- Parse last argument *on the same level*
             arg <- parseExprMixfix (currentLevel : levels) table
             rec (PushArgument status' arg)

      -- Case 2: Finished operator
      | statusIsCompleteOperator status' currentLevel
        = let op = statusOperatorName status'
              pos = statusPosition status'
              result = foldr (flip (EApp pos))
                             (EVar pos op)
                             (statusArguments status')
            in do b <- isLeftAssocM op
                  if b
                   then -- Left-associative operator:
                        -- restart on the same level with the
                        -- accumulated expression.
                        parseExprInfix
                          (currentLevel : levels)
                          table
                          (PushArgument (EmptyStatus pos) result)
                   else -- Non-associative or right-associative operator
                        return result
      -- Case 3: Unfinished
      | otherwise =
          parseExprInfix (currentLevel : levels) table status'


    -- Check if status is an "almost complete" right-associative operator
    -- (exactly one operator pending).
    statusIsAlmostCompleteRightOperator :: Status -> PrecedenceLevel -> Bool
    statusIsAlmostCompleteRightOperator status level =
      let moduleName = statusModuleName status
          parts      = statusOperatorParts status
       in case moduleName of
            Nothing -> False
            Just m  ->
              let fullOperatorName = qualify m (concat (parts ++ ["_"])) in
                any (operatorIsEqual moduleName (parts ++ ["_"]))
                    (S.toList level)
                &&
                isRightAssoc fullOperatorName table

    -- Check if status is a complete operator
    statusIsCompleteOperator :: Status -> PrecedenceLevel -> Bool
    statusIsCompleteOperator status level =
      let moduleName = statusModuleName status
          parts      = statusOperatorParts status
       in any (operatorIsEqual moduleName parts)
              (S.toList level)

    fromJustOrFail :: Maybe a -> M a
    fromJustOrFail Nothing = do
      state <- getFS
      failM InternalError "INTERNAL ERROR: Extend isEndOfExpression ?"
    fromJustOrFail (Just x) = return x

    isEndOfExpression :: M Bool
    isEndOfExpression = do
      t <- peekType
      return $ case t of
        T_EOF       -> True
        T_RParen    -> True
        T_RBrace    -> True
        T_Semicolon -> True
        T_Eq        -> True
        T_Colon     -> True
        --T_Where   -> True  -- maybe add later
        _           -> False

    statusPosition :: Status -> Position
    statusPosition (EmptyStatus pos)           = pos
    statusPosition (PushArgument status _)     = statusPosition status
    statusPosition (PushOperatorPart status _) = statusPosition status

    statusIsValidPrefixInLevel :: Status -> PrecedenceLevel -> Bool
    statusIsValidPrefixInLevel (EmptyStatus _)                  _ = True
    statusIsValidPrefixInLevel (PushArgument (EmptyStatus _) _) _ = True
    statusIsValidPrefixInLevel status level =
      let moduleName = statusModuleName status
          parts      = statusOperatorParts status
       in (case moduleName of
             Nothing -> True
             Just m  -> statusModuleNamesCoherent m status) &&
          any (operatorIsPrefix moduleName parts) (S.toList level)

    statusModuleName :: Status -> Maybe QName
    statusModuleName (EmptyStatus _)        = Nothing
    statusModuleName (PushOperatorPart _ q) = Just $ moduleNameFromQName q
    statusModuleName (PushArgument s _)     = statusModuleName s

    statusModuleNamesCoherent :: QName -> Status -> Bool
    statusModuleNamesCoherent m (EmptyStatus _) = True
    statusModuleNamesCoherent m (PushOperatorPart _ q) =
      m == moduleNameFromQName q
    statusModuleNamesCoherent m (PushArgument s _) =
      statusModuleNamesCoherent m s

    statusOperatorParts :: Status -> [String]
    statusOperatorParts (EmptyStatus _)        = [] 
    statusOperatorParts (PushOperatorPart s q) =
      statusOperatorParts s ++ [unqualifiedName q]
    statusOperatorParts (PushArgument s _)     =
      statusOperatorParts s ++ ["_"]

    statusOperatorName :: Status -> QName
    statusOperatorName status =
      case statusModuleName status of
        Nothing -> error "Status does not match an operator."
        Just m  -> qualify m (concat (statusOperatorParts status))

    statusArguments :: Status -> [Expr]
    statusArguments (EmptyStatus _)             = []
    statusArguments (PushOperatorPart status _) = statusArguments status
    statusArguments (PushArgument status arg)   = arg : statusArguments status

    operatorMatches :: ([String] -> [String] -> Bool)
                    -> Maybe QName -> [String] -> QName -> Bool
    operatorMatches f moduleName parts targetOperator =
        let targetOperatorModule = moduleNameFromQName targetOperator
            targetOperatorParts  = splitParts (unqualifiedName targetOperator)
         in (case moduleName of
              Nothing -> True
              Just m  -> m == targetOperatorModule) &&
            f parts targetOperatorParts

    operatorIsPrefix :: Maybe QName -> [String] -> QName -> Bool
    operatorIsPrefix = operatorMatches isPrefixOf

    operatorIsEqual :: Maybe QName -> [String] -> QName -> Bool
    operatorIsEqual = operatorMatches (==)

{-- End mixfix parser --}

parseApplication :: M Expr
parseApplication = parseAtom -- TODO

parseAtom :: M Expr
parseAtom = do
  t <- peekType
  case t of
    T_Id _   -> do pos    <- currentPosition
                   qname  <- parseAndResolveQName
                   isOp   <- isOperatorPartM qname 
                   if isOp
                    then failM ParseError
                               ("Operator part: " ++ show qname ++
                                " cannot be used as a variable.")
                    else return $ EVar pos qname
    T_Int n  -> do pos    <- currentPosition
                   getToken
                   return $ EInt pos n
    T_LParen -> do match T_LParen
                   expr   <- parseExpr
                   match T_RParen
                   return expr

    -- TODO: other kinds of expressions
    _      -> failM ParseError
                     ("Expected an expression.\n" ++
                      "Got: " ++ show t ++ ".")

