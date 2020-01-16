module Lexer (
  Token(..),
  TokenType(..),
  Puntuation(..),
  KeyWord(..),
  TPosition(..),
  Point(..),
  tokenizeProgram,
) where

import Utils
import Point
import TContext
import Error

data Token = Token { position :: TPosition , tokenType :: TokenType } deriving (Eq, Show)

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

consumeWord :: TContext -> String -> (String, TContext)
consumeWord c ""                                 = ("", c)
consumeWord c ('=':'>':xs)                       = ("=>", incrementColumn 2 c)
consumeWord c w@(x:xs) | isPuntuation (x:"")     = (x:"", incrementColumn 1 c)
                       | otherwise               =  let word = nextWord w
                                                    in (word, incrementColumn (toInteger (length word)) c)

------------------------------

tokenizeProgram :: String -> String -> [Token]
tokenizeProgram fileName source = tokenizeProgramWithContext (context fileName source) source

tokenizeProgramWithContext :: TContext -> String -> [Token]
tokenizeProgramWithContext context ""           = []
tokenizeProgramWithContext context (' ':xs)     = tokenizeProgramWithContext (incrementColumn 1 context) xs
tokenizeProgramWithContext context ('\n':xs)    = tokenizeProgramWithContext (incrementRow context) xs
tokenizeProgramWithContext context ('-':'-':xs) = tokenizeProgramWithContext (incrementRow context) (removeLine xs)
tokenizeProgramWithContext context source       = let (word, context') = consumeWord context source
                                                  in let l = toInteger (length word)
                                                     in tokenizeWordWithContext word context (incrementIndex l context') source

tokenizeWordWithContext :: String -> TContext -> TContext -> String -> [Token]
tokenizeWordWithContext word previousContext nextContext source =  let recursiveResult = tokenizeProgramWithContext nextContext (drop (length word) source)
                                                                   in let t =  if isKeyword word
                                                                                then token previousContext nextContext (KToken (tokenizeKeyword word))
                                                                                else  if isPuntuation word
                                                                                      then token previousContext nextContext (PToken (tokenizePuntuation word))
                                                                                      else  if isNumeric word
                                                                                            then token previousContext nextContext (NToken (read word :: Int))
                                                                                            else token previousContext nextContext (IDToken word)
                                                                      in t : recursiveResult

tokenizeKeyword :: String -> KeyWord
tokenizeKeyword "where"    = Where
tokenizeKeyword "module"   = Module
tokenizeKeyword "let"      = Let
tokenizeKeyword "in"       = In
tokenizeKeyword "import"   = Import
tokenizeKeyword "\\"       = Backslash
tokenizeKeyword "data"     = Data
tokenizeKeyword "_"        = Underscore
tokenizeKeyword "class"    = Class
tokenizeKeyword "type"     = Type
tokenizeKeyword "instance" = Instance

tokenizePuntuation :: String -> Puntuation
tokenizePuntuation "("  = LeftParen
tokenizePuntuation ")"  = RightParen
tokenizePuntuation "{"  = LeftBrace
tokenizePuntuation "}"  = RightBrace
tokenizePuntuation ";"  = SemiColon
tokenizePuntuation ":"  = Colon
tokenizePuntuation "="  = Equal
tokenizePuntuation "=>" = Arrow
