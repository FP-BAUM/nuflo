module Parser.Parser(parse) where

import Error(Error(..), ErrorType(..))
import Position(Position(..), unknownPosition)
import FailState(FailState, getFS, putFS, evalFS, failFS)
import Lexer.Name(QName(..), readName)
import Lexer.Token(Token(..), TokenType(..))
import Parser.AST(Program(..),
                  AnnDeclaration(..), Declaration,
                  AnnExpr(..), Expr)
import Parser.ModuleSystem()

parse :: [Token] -> Either Error Program
parse tokens = evalFS parseM initialState
  where initialState = ParserState {
                         stateInput    = tokens,
                         stateLocation = tokensLocation tokens unknownLocation
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
locationPosition (Location _ p False) = p

locationAtEnd :: Location -> Location
locationAtEnd (Location p1 p2 _)  = Location p1 p2 False

---- Parser monad

data ParserState = ParserState {
                     stateInput    :: [Token],
                     stateLocation :: Location
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
  match T_Where
  --TODO: enter module
  match T_LBrace
  --TODO: read imports
  decls <- parseDeclarations
  match T_RBrace
  return decls

parseBareModule :: M [Declaration]
parseBareModule = do
  --TODO: enter "Main" module
  match T_LBrace
  --TODO: read imports
  decls <- parseDeclarations
  match T_RBrace
  return decls

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
      d <- parseDeclaration
      t' <- peekType
      case t' of
        T_Semicolon -> do
          ds <- parseDeclarations
          return (d : ds)
        T_RBrace    -> return [d]
        _           -> failM ParseError
                             ("Expected \";\" or \"}\".\n" ++
                              "Got: " ++ show t' ++ ".")

parseDeclaration :: M Declaration --TODO
parseDeclaration = do
  t <- peekType
  case t of
    T_Data -> parseDataDeclaration
    -- TODO: other kinds of declarations
    T_Id _ -> parseTypeSignatureOrValueDeclaration
    _      -> failM ParseError
                     ("Expected a declaration.\n" ++
                      "Got: " ++ show t ++ ".")

parseDataDeclaration :: M Declaration   --TODO
parseDataDeclaration = return undefined --TODO

parseTypeSignatureOrValueDeclaration :: M Declaration --TODO
parseTypeSignatureOrValueDeclaration = do
  pos <- currentPosition
  expr1 <- parseExpr
  --TODO: if expr is single variable followed by DOT, parse type signature
  match T_Eq
  expr2 <- parseExpr
  return (ValueDeclaration pos expr1 expr2)

parseExpr :: M Expr
parseExpr = do
  t <- peekType
  case t of
    T_Id _ -> do pos   <- currentPosition
                 qname <- parseQName
                 return $ EVar pos qname
    -- TODO: other kinds of expressions
    _      -> failM ParseError
                     ("Expected an expression.\n" ++
                      "Got: " ++ show t ++ ".")

