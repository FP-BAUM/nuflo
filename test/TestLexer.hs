
module TestLexer(tests) where

import Test(TestSuite(..), Test(..))

import Lexer

tokenize_test :: String -> String -> [Token] -> Test
tokenize_test description source expectedTokens =
  TestCase description expectedTokens (tokenizeProgram "test" source)

tests :: TestSuite
tests = TestSuite "LEXER" [
  tokenize_test "Empty program" "" [],
  -- Numbers
  tokenize_test "Number 42 to a NToken" "42" [Token (TPosition (Point 0 0 0) (Point 0 0 0)) (NToken 42) ],
  tokenize_test "Number 01 to a NToken" "01" [Token (TPosition (Point 0 0 0) (Point 0 0 0)) (NToken 01) ],
  tokenize_test "Number 10 to a NToken" "10" [Token (TPosition (Point 0 0 0) (Point 0 0 0)) (NToken 10) ],
  -- Identifiers
  tokenize_test "ID if to IDToken" "if" [Token (TPosition (Point 0 0 0) (Point 0 0 0)) (IDToken "if") ],
  tokenize_test "ID then to IDToken" "then"  [Token (TPosition (Point 0 0 0) (Point 0 0 0)) (IDToken "then") ],
  tokenize_test "ID function to IDToken" "function" [Token (TPosition (Point 0 0 0) (Point 0 0 0)) (IDToken "function") ],
  -- Keywords
  tokenize_test "Keyword Where to KToken" "where" [Token (TPosition (Point 0 0 0) (Point 0 0 0)) (KToken Where) ],
  tokenize_test "Keyword Module to KToken" "module" [Token (TPosition (Point 0 0 0) (Point 0 0 0)) (KToken Module) ],
  tokenize_test "Keyword Let to KToken" "let" [Token (TPosition (Point 0 0 0) (Point 0 0 0)) (KToken Let) ],
  tokenize_test "Keyword In to KToken" "in" [Token (TPosition (Point 0 0 0) (Point 0 0 0)) (KToken In) ],
  tokenize_test "Keyword Import to KToken" "import" [Token (TPosition (Point 0 0 0) (Point 0 0 0)) (KToken Import) ],
  tokenize_test "Keyword Backslash to KToken" "\\" [Token (TPosition (Point 0 0 0) (Point 0 0 0)) (KToken Backslash) ],
  tokenize_test "Keyword Data to KToken" "data" [Token (TPosition (Point 0 0 0) (Point 0 0 0)) (KToken Data) ],
  tokenize_test "Keyword Underscore to KToken" "_" [Token (TPosition (Point 0 0 0) (Point 0 0 0)) (KToken Underscore) ],
  tokenize_test "Keyword Class to KToken" "class" [Token (TPosition (Point 0 0 0) (Point 0 0 0)) (KToken Class) ],
  tokenize_test "Keyword Type to KToken" "type" [Token (TPosition (Point 0 0 0) (Point 0 0 0)) (KToken Type) ],
  tokenize_test "Keyword Instance to KToken" "instance" [Token (TPosition (Point 0 0 0) (Point 0 0 0)) (KToken Instance) ],
  -- Puntuation
  tokenize_test "Puntuation LeftParen to a PToken" "(" [Token (TPosition (Point 0 0 0) (Point 0 0 0)) (PToken LeftParen) ],
  tokenize_test "Puntuation RightParen to a PToken" ")" [Token (TPosition (Point 0 0 0) (Point 0 0 0)) (PToken RightParen) ],
  tokenize_test "Puntuation LeftBrace to a PToken" "{" [Token (TPosition (Point 0 0 0) (Point 0 0 0)) (PToken LeftBrace) ],
  tokenize_test "Puntuation RightBrace to a PToken" "}" [Token (TPosition (Point 0 0 0) (Point 0 0 0)) (PToken RightBrace) ],
  tokenize_test "Puntuation SemiColon to a PToken" ";" [Token (TPosition (Point 0 0 0) (Point 0 0 0)) (PToken SemiColon) ],
  tokenize_test "Puntuation Colon to a PToken" ":" [Token (TPosition (Point 0 0 0) (Point 0 0 0)) (PToken Colon) ],
  tokenize_test "Puntuation Equal to a PToken" "=" [Token (TPosition (Point 0 0 0) (Point 0 0 0)) (PToken Equal) ],
  tokenize_test "Puntuation Arrow to a PToken" "=>" [Token (TPosition (Point 0 0 0) (Point 0 0 0)) (PToken Arrow) ]]
  -- Ignore comments
                        -- TODO
  -- tokenize_test "Ignoring single comment" "--this is a comment" [],
  -- tokenize_test "Ignoring comment between lines" "a b\n--c d e f\ng" [[IDToken "a", IDToken "b"], [IDToken "g"]],
  -- tokenize_test "Ignoring complex comments" "module Lexer where --where\n--this is a comment\nid a = a --identity" (tokenizeProgram "module Lexer where\nid a = a"),
  -- Complex programs
  -- tokenize_test "Tokenize a complex program with numbers and identifiers" "42 if \n then else def 44" [[NToken 42, IDToken "if"], [IDToken "then", IDToken "else", IDToken "def", NToken 44]]
  -- ]
