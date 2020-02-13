module Lexer.Token (
  Token(..),
  TokenType(..),
) where

import Position(Position)

data Token = Token {
               tokenStartPos :: Position,
               tokenEndPos   :: Position,
               tokenType     :: TokenType
             }

data TokenType =
    T_Int Integer
  | T_Id String
  -- Keywords
  | T_Class
  | T_Data
  | T_Import
  | T_In
  | T_Instance
  | T_Lambda
  | T_Let
  | T_Module
  | T_Where
  -- Punctuation
  | T_LParen
  | T_RParen
  | T_LBrace
  | T_RBrace
  | T_Semicolon
  | T_Underscore
  deriving (Eq, Show)

instance Show Token where
  show token = show (tokenType token)

