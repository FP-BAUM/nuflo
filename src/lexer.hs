module Lexer ( tokenizeProgram, Token ) where

import Text.Read

data Token =  PToken Puntuation |
              NToken Int        |
              IDToken String    |
              KToken KeyWord deriving (Show, Eq)

data Puntuation = LeftParen | RightParen | -- ( | )
                  LeftBrace | RightBrace | -- { | }
                  SemiColon | Colon      | -- ; | :
                  Equal     | Arrow        -- = | =>
                  deriving (Show, Eq)

data KeyWord = Where | Let | In | Import | Backslash | -- where | let | in | import | \
               Data | Underscore | Class | Type | Instance -- data | _ | class | type | instance
               deriving (Show, Eq)

type TokenLine = [Token]

tokenizeProgram :: String -> [TokenLine]
tokenizeProgram = tokenizeLines . lines

tokenizeLines :: [String] -> [TokenLine]
tokenizeLines = map tokenizeLine

tokenizeLine :: String -> TokenLine
tokenizeLine = (map tokenize) . words

tokenize :: String -> Token
-- TODO: missing implementation
tokenize = IDToken 