module Lexer (
  Token(..),
  TokenType(..),
  Puntuation(..),
  KeyWord(..),
  TPosition(..),
  Point(..),
  TokenLine,
  tokenizeProgram
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

-----------------------------

tokenizeProgram :: String -> String -> [Token]
tokenizeProgram fileName source = tokenizeProgramWithContext (context fileName source) source

tokenizeProgramWithContext :: TContext -> String -> [Token]
tokenizeProgramWithContext context "" = []
tokenizeProgramWithContext context ('\n':xs) = tokenizeProgramWithContext (incrementLine context) xs
tokenizeProgramWithContext context ('-':'-':xs) = tokenizeProgramWithContext ((incrementLine . incrementIndex 2) context) (removeLine xs)
-- tokenizeProgramWithContext context source = let word = nextWord source
--                                         in let l = length word
--                                           in if isKeyword word
--                                              then token context (KToken (tokenizeKeyword word)) : tokenizeProgramWithContext (incrementIndex l context) (drop l source)
--                                              else if isNumeric word
--                                                   then token context (NToken (read word :: Int)) : tokenizeProgramWithContext (incrementIndex l context) (drop l source)
--                                                   else 



-- tokenize :: String -> TokenType
-- tokenize s
--   | isNumeric s = NToken (read s :: Int)
--   | isKeyword s = KToken (tokenizeKeyword s)
--   | isPuntuation s = PToken (tokenizePuntuation s)
--   | otherwise = IDToken s

tokenizeKeyword :: String -> KeyWord
tokenizeKeyword "where" = Where
tokenizeKeyword "module" = Module
tokenizeKeyword "let" = Let
tokenizeKeyword "in" = In
tokenizeKeyword "import" = Import
tokenizeKeyword "\\" = Backslash
tokenizeKeyword "data" = Data
tokenizeKeyword "_" = Underscore
tokenizeKeyword "class" = Class
tokenizeKeyword "type" = Type
tokenizeKeyword "instance" = Instance

tokenizePuntuation :: String -> Puntuation
tokenizePuntuation "(" = LeftParen
tokenizePuntuation ")" = RightParen
tokenizePuntuation "{" = LeftBrace
tokenizePuntuation "}" = RightBrace
tokenizePuntuation ";" = SemiColon
tokenizePuntuation ":" = Colon
tokenizePuntuation "=" = Equal
tokenizePuntuation "=>" = Arrow
