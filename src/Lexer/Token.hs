module Lexer.Token (
  Token(..),
  TokenType(..),
  OffsideType(..),
  Puntuation(..),
  KeyWord(..),
  TPosition(..),
  Point(..),
  token,
  positionFromContext,
) where

import Lexer.Point
import Lexer.TContext

data Token = Token { position :: TPosition , tokenType :: TokenType } | OffsideToken OffsideType Int deriving (Eq, Show)

data OffsideType =  Note1 | -- <>
                    Note2   -- {}
                    deriving (Eq, Show)

data TPosition = TPosition { start :: Point, end :: Point } deriving (Eq, Show)

data TokenType =  PToken Puntuation |
                  NToken Int        |
                  IDToken String    |
                  KToken KeyWord deriving (Eq, Show)

data Puntuation = LeftParen | RightParen | -- ( | )
                  LeftBrace | RightBrace | -- { | }
                  SemiColon | Colon      | -- ; | :
                  Equal     | Arrow        -- = | =>
                  deriving (Eq, Show)

data KeyWord = Where | Let | In | Import | Backslash |              -- where | let | in | import | \
               Data | Underscore | Class | Type | Instance | Module -- data | _ | class | type | instance | module
               deriving (Eq, Show)

------------------------------

positionFromContext :: TContext -> TContext -> TPosition
positionFromContext context nextContext = TPosition (pointFromContext context) (pointFromContext nextContext)

token :: TContext -> TContext -> TokenType -> Token
token context nextContext tokenType = Token (positionFromContext context nextContext) tokenType