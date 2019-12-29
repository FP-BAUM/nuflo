module Lexer ( tokenizeProgram, Token(..) ) where

import Text.Read

data Token =  PToken Puntuation |
              NToken Int        |
              IDToken String    |
              KToken KeyWord deriving (Eq, Show)

data Puntuation = LeftParen | RightParen | -- ( | )
                  LeftBrace | RightBrace | -- { | }
                  SemiColon | Colon      | -- ; | :
                  Equal     | Arrow        -- = | =>
                  deriving (Eq, Show)

data KeyWord = Where | Let | In | Import | Backslash | -- where | let | in | import | \
               Data | Underscore | Class | Type | Instance -- data | _ | class | type | instance
               deriving (Eq, Show)

type TokenLine = [Token]

tokenizeProgram :: String -> [TokenLine]
tokenizeProgram = (map tokenizeLine) . lines

tokenizeLine :: String -> TokenLine
tokenizeLine = (map tokenize) . words

tokenize :: String -> Token
-- TODO: missing implementation
tokenize = IDToken 