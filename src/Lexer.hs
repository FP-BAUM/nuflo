module Lexer (
  Token(..),
  TokenType(..),
  Puntuation(..),
  KeyWord(..),
  TPosition(..),
  Point(..),
  TokenLine,
  tokenizeProgram,
) where

import Utils

data Token = Token { position :: TPosition , tokenType :: TokenType } deriving (Eq, Show)

data TPosition = TPosition { start :: Point, end :: Point } deriving (Eq, Show)

data Point = Point { row :: Integer, column :: Integer, index :: Integer } deriving (Eq, Show) -- index is the number of the current character respect the complete file

data TContext = TContext { fileName :: String , source :: String , tIndex :: Point } deriving (Eq, Show)

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

type TokenLine = [TokenType]

-- Structure aux functions
-- TODO: this functions should be splited in differents files, one for each data structure
emptyPoint :: Point
emptyPoint = Point 0 0 0

newLine :: Point -> Point
newLine Point { row = r, column = c, index = i } = Point (r + 1) c i

updateIndex :: Integer -> Point -> Point
updateIndex n Point { row = r, column = c, index = i } = Point r c (i + n)

context :: String -> String -> TContext
context name source = TContext name source emptyPoint

applyIndexContext :: (Point -> Point) -> TContext -> TContext
applyIndexContext f (TContext {fileName = name, source = s, tIndex = point}) = TContext name s (f point)

incrementLine :: TContext -> TContext
incrementLine = applyIndexContext newLine

incrementIndex :: Integer -> TContext -> TContext
incrementIndex n = applyIndexContext (updateIndex n)

removeLine :: String -> String
removeLine "" = ""
removeLine ('\n':xs) = xs
removeLine (_:xs) = removeLine xs

pointFromContext :: TContext -> Point
pointFromContext TContext {fileName = name, source = s, tIndex = point} = point

positionFromContext :: TContext -> TContext -> TPosition
positionFromContext context nextContext = TPosition (pointFromContext context) (pointFromContext nextContext)

token :: TContext -> TContext -> TokenType -> Token
token context nextContext tokenType = Token (positionFromContext context nextContext) tokenType

-----------------------------

tokenizeProgram :: String -> String -> [Token]
tokenizeProgram fileName source = tokenizeProgramWithContext (context fileName source) source

tokenizeProgramWithContext :: TContext -> String -> [Token]
tokenizeProgramWithContext context ""           = []
tokenizeProgramWithContext context (' ':xs)     = tokenizeProgramWithContext context xs
tokenizeProgramWithContext context ('\n':xs)    = tokenizeProgramWithContext (incrementLine context) xs
tokenizeProgramWithContext context ('-':'-':xs) = tokenizeProgramWithContext ((incrementLine . incrementIndex 2) context) (removeLine xs)
tokenizeProgramWithContext context source       = let word = consumeWord source
                                                  in let l = toInteger (length word)
                                                     in tokenizeWordWithContext word l context source

consumeWord :: String -> String
consumeWord ""                             = ""
consumeWord ('=':'>':xs)                   = "=>"
consumeWord w@(x:xs) | isPuntuation (x:"") = x:""
                     | otherwise           = nextWord w

nextWord :: String -> String
nextWord ""                                                      = ""
nextWord (x:xs) | (x == ' ' || x == '\n' || isPuntuation (x:"")) = ""
                | otherwise                                      = x : nextWord xs

tokenizeWordWithContext :: String -> Integer -> TContext -> String -> [Token]
tokenizeWordWithContext word l context source =  let nextContext = incrementIndex l context
                                                      in let recursiveResult = tokenizeProgramWithContext nextContext (drop (length word) source)
                                                          in let t =  if isKeyword word
                                                                      then token context nextContext (KToken (tokenizeKeyword word))
                                                                      else  if isPuntuation word
                                                                            then token context nextContext (PToken (tokenizePuntuation word))
                                                                            else  if isNumeric word
                                                                                  then token context nextContext (NToken (read word :: Int))
                                                                                  else token context nextContext (IDToken word)
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
