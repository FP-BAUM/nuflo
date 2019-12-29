module Lexer ( tokenizeProgram, Token(..), Puntuation(..), KeyWord(..), TokenLine) where

import Utils

data Token =  PToken Puntuation |
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

type TokenLine = [Token]

tokenizeProgram :: String -> [TokenLine]
tokenizeProgram = (map tokenizeLine) . lines

tokenizeLine :: String -> TokenLine
tokenizeLine = (map tokenize) . words

tokenize :: String -> Token
tokenize s
  | isNumeric s = NToken (read s :: Int)
  | isKeyword s = KToken (tokenizeKeyword s)
  | isPuntuation s = PToken (tokenizePuntuation s)
  | otherwise = IDToken s

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
