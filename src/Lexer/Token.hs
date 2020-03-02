module Lexer.Token (
  Token(..),
  TokenType(..),
  isLBrace, isRBrace, isId, isDot, isModule
) where

import Position(Position)

data Token = Token {
               tokenStartPos :: Position,
               tokenEndPos   :: Position,
               tokenType     :: TokenType
             }

data TokenType =
    T_EOF
  | T_Int Integer
  | T_Id String
  -- Keywords
  | T_As
  | T_Class
  | T_Colon
  | T_Data
  | T_Eq
  | T_Import
  | T_In
  | T_Infix
  | T_Infixl
  | T_Infixr
  | T_Instance
  | T_Lambda
  | T_Let
  | T_Module
  | T_Type
  | T_Where
  -- Punctuation
  | T_Dot
  | T_LParen
  | T_RParen
  | T_LBrace
  | T_RBrace
  | T_Semicolon
  deriving (Eq, Show)

instance Show Token where
  show token = show (tokenType token)

---

isId :: Token -> Bool
isId t =
  case tokenType t of
    T_Id _ -> True
    _      -> False

isLBrace :: Token -> Bool
isLBrace t =
  case tokenType t of
    T_LBrace -> True
    _        -> False

isRBrace :: Token -> Bool
isRBrace t =
  case tokenType t of
    T_RBrace -> True
    _        -> False

isModule :: Token -> Bool
isModule t =
  case tokenType t of
    T_Module -> True
    _        -> False

isDot :: Token -> Bool
isDot t =
  case tokenType t of
    T_Dot -> True
    _     -> False

