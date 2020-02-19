module Parser.Parser(parse) where

import Error(Error(..), ErrorType(..), ErrorMessage)
import Position(Position(..), unknownPosition)
import FailState(FailState, getFS, modifyFS, putFS, evalFS, failFS)
import Lexer.Name(QName(..), readName, qualify, unqualifiedName)
import Lexer.Token(Token(..), TokenType(..))
import Parser.AST(
         Program(..), AnnDeclaration(..), Declaration, AnnExpr(..), Expr,
         exprIsVariable, exprHeadVariable
       )
import Parser.ModuleSystem(
         Module,
           emptyModule, addSubmodule, exportAllNamesFromModule, exportNames,
           declareName,
         Context,
           emptyContext, contextCurrentModuleName,
           resolveName, importAllNamesFromModule, importNames
       )

parse :: [Token] -> Either Error Program
parse tokens = evalFS parseM initialState
  where initialState =
          ParserState {
            stateInput       = tokens,
            stateLocation    = tokensLocation tokens unknownLocation,
            stateRootModule  = emptyModule,
            stateNameContext = error "Empty name context."
          }

---- Location

data Location = Location {
                  locationStartPos :: Position,
                  locationEndPos   :: Position,
                  locationFlag     :: Bool      -- start or end?
                }

unknownLocation :: Location
unknownLocation = Location unknownPosition unknownPosition True

makeLocation :: Position -> Position -> Location
makeLocation p1 p2 = Location p1 p2 True

locationPosition :: Location -> Position
locationPosition (Location p _ True)  = p
locationPosition (Location p _ False) = p

locationAtEnd :: Location -> Location
locationAtEnd (Location p1 p2 _)  = Location p1 p2 False

---- Parser monad

data ParserState = ParserState {
                     stateInput       :: [Token],
                     stateLocation    :: Location,
                     stateRootModule  :: Module,
                     stateNameContext :: Context
                   }

type M = FailState ParserState

tokensLocation :: [Token] -> Location -> Location
tokensLocation tokens elseLoc =
  if null tokens
   then elseLoc
   else makeLocation (tokenStartPos tok) (tokenEndPos tok)
  where tok = head tokens

peek :: M Token
peek = do
  state <- getFS
  pos   <- currentPosition
  putFS (state {
           stateLocation = locationAtEnd (stateLocation state)
         })
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
                   stateLocation = tokensLocation toks (stateLocation state)
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
  return $ locationPosition (stateLocation state)

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

parseId :: M String
parseId = do
  t <- peekType
  case t of
    T_Id id -> do getToken
                  return id
    _       -> failM ParseError
                     ("Expected an identifier.\n" ++
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

parseDataDeclaration :: M Declaration   --TODO
parseDataDeclaration = return undefined --TODO

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
  t <- peekType
  case t of
    T_Id _  -> do pos    <- currentPosition
                  qname  <- parseAndResolveQName
                  return $ EVar pos qname
    T_Int n -> do pos    <- currentPosition
                  getToken
                  return $ EInt pos n
    -- TODO: other kinds of expressions
    _      -> failM ParseError
                     ("Expected an expression.\n" ++
                      "Got: " ++ show t ++ ".")

