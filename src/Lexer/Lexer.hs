module Lexer.Lexer (tokenize) where

import FailState(FailState, getFS, modifyFS, failFS, evalFS)
import Error(Error(..), ErrorType(..))
import Position(Position, initialPosition, updatePosition)

import Lexer.Token(Token(..), TokenType(..))
import Lexer.Categories(
         isDigit, isInteger, isWhitespace,
         isPunctuation, punctuationType,
         isKeyword, keywordType, isIdent
       )
import Lexer.Layout(layout)

tokenize :: FilePath -> String -> Either Error [Token]
tokenize filename source = do
    tokens <- evalFS (tokenizeM source) initialState
    layout tokens
  where initialState = LexerState { pos = initialPosition filename source }

---- Lexer monad

data LexerState = LexerState { pos :: Position }
type M = FailState LexerState

consumeString :: String -> M ()
consumeString s = modifyFS (\ state -> state {
                     pos = updatePosition s (pos state)
                   })

consumeChar :: Char -> M ()
consumeChar c = consumeString [c]

getPos :: M Position
getPos = do state <- getFS
            return (pos state)

---- Tokenizer

tokenizeM :: String -> M [Token]
tokenizeM ""                           = return []
tokenizeM cs@(c : _) | isWhitespace c  = ignoreWhitespace cs
tokenizeM cs@('-' : '-' : _)           = ignoreSingleLineComment cs
tokenizeM cs@('{' : '-' : _)           = ignoreMultiLineComment cs
tokenizeM cs@(c : _) | isPunctuation c = readPunctuation cs
tokenizeM cs@(c : _) | isIdent c       = readNamePart cs
tokenizeM (c : _)                      =
  failM LexerErrorInvalidCharacter
        ("Invalid character: '" ++ show c ++ "'.")

ignoreWhitespace :: String -> M [Token]
ignoreWhitespace (c : cs) = do
  consumeChar c
  tokenizeM cs

ignoreSingleLineComment :: String -> M [Token]
ignoreSingleLineComment ('\n' : cs) = do
  consumeChar '\n'
  tokenizeM cs
ignoreSingleLineComment (c : cs)    = do
  consumeChar c
  ignoreSingleLineComment cs

ignoreMultiLineComment :: String -> M [Token]
ignoreMultiLineComment cs = rec 0 cs
  where
    rec n ('{' : '-' : cs) = do
      consumeString "{-"
      rec (n + 1) cs
    rec n ('-' : '}' : cs) = do
      consumeString "-}"
      if n == 1
       then tokenizeM cs
       else rec (n - 1) cs 
    rec n (c : cs) = do
      consumeChar c
      rec n cs

readPunctuation :: String -> M [Token]
readPunctuation (c : cs) = do
  start <- getPos
  consumeChar c
  end <- getPos
  let t = Token start end (punctuationType c)
   in do ts <- tokenizeM cs
         return (t : ts)

readNamePart :: String -> M [Token]
readNamePart cs = do
  start <- getPos
  let (ident, cs') = span isIdent cs in do
    consumeString ident
    end <- getPos
    let t = Token start end (nameType ident) in do
      ts <- tokenizeM cs'
      return (t : ts)
  where
    nameType ident
      | isKeyword ident = keywordType ident
      | isInteger ident = T_Int (read ident :: Integer)
      | otherwise       = T_Id ident

failM :: ErrorType -> String -> M a
failM errorType msg = do
  pos <- getPos
  failFS (Error errorType pos msg)

