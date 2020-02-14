module Parser.Parser(parse) where

import Error(Error(..), ErrorType(..))
import Position(Position(..), unknownPosition)
import FailState(FailState, getFS, putFS, evalFS, failFS)
import Lexer.Name(QName(..), readName)
import Lexer.Token(Token(..), TokenType(..))
import Parser.AST(Program(..))

parse :: [Token] -> Either Error Program
parse tokens = evalFS parseM initialState
  where initialState = ParserState {
                         stateInput    = tokens,
                         statePosition = tokensPosition tokens unknownPosition
                       }

---- Parser monad

data ParserState = ParserState {
                     stateInput    :: [Token],
                     statePosition :: Position
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

getToken :: M Token
getToken = do
  state <- getFS
  pos   <- currentPosition
  if null (stateInput state)
   then return $ Token pos pos T_EOF
   else let (tok : toks) = stateInput state in do
          putFS (state {
                   stateInput    = toks,
                   statePosition = tokensPosition toks (tokenEndPos tok)
                 })
          return tok

match :: TokenType -> M ()
match typ = do
  typ' <- peekType
  if typ == typ'
   then do getToken
           return ()
   else failM ParseError
              ("Expected: " ++ show typ ++ ".\n" ++
               "Got     : " ++ show typ' ++ ".")

currentPosition :: M Position
currentPosition = do
  state <- getFS
  return $ statePosition state

failM :: ErrorType -> String -> M a
failM errorType msg = do
  pos <- currentPosition
  failFS (Error errorType pos msg)

---- Parser

parseM :: M Program
parseM = do
  parseModules 
  return $ Program {
             programDeclarations = []
           }

parseModules :: M ()
parseModules = do
  t <- peekType
  case t of
    T_Module -> parseModule
    T_LBrace -> parseBareModule
    T_EOF    -> return ()
    _        -> failM ParseError
                      ("Expected a module, but got: " ++ show t ++ ".")

parseModule :: M ()
parseModule = do
  match T_Module
  qname <- parseQName
  return ()            -- TODO

parseBareModule :: M ()
parseBareModule = do
  match T_LBrace
  return ()            -- TODO

parseQName :: M QName  -- TODO: allow qualifiers
parseQName = do
  id <- parseId
  return $ readName id

parseId :: M String
parseId = do
  typ <- peekType
  case typ of
    T_Id id -> return id
    _       -> failM ParseError
                     ("Expected an identifier.\n" ++
                      "Got     : " ++ show typ ++ ".")

