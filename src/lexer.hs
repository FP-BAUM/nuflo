module Lexer ( tokenize, Token ) where

data Token = PToken Puntuation | NumToken Int | IDToken String | KToken KeyWord

data Puntuation = LeftParen | RightParen | -- ( | )
                  LeftBrace | RightBrace | -- { | }
                  SemiColon | Colon      | -- ; | :
                  Equal     | Arrow        -- = | =>

data KeyWord = Where | Let | In | Import | Backslash | -- where | let | in | import | \
               Data | Underscore | Class | Type | Instance -- data | _ | class | type | instance

tokenize :: String -> [Token]
tokenize _ = []
