
module TestLexer(tests) where

import Test(TestSuite(..), Test(..))

import Lexer

tokenize_test :: String -> String -> [Token] -> Test
tokenize_test description source expectedTokens =
  TestCase description (tokenizeProgram "Test" source) expectedTokens

tests :: TestSuite
tests = TestSuite "LEXER" [
  tokenize_test "Empty program" "" [],
  -- Numbers
  tokenize_test "Number 42 to a NToken" "42" [Token (TPosition (Point 0 0 0) (Point 0 2 2)) (NToken 42) ],
  tokenize_test "Number 01 to a NToken" "01" [Token (TPosition (Point 0 0 0) (Point 0 2 2)) (NToken 01) ],
  tokenize_test "Number 10 to a NToken" "10" [Token (TPosition (Point 0 0 0) (Point 0 2 2)) (NToken 10) ],
  -- Identifiers
  tokenize_test "ID if to IDToken" "if" [Token (TPosition (Point 0 0 0) (Point 0 2 2)) (IDToken "if") ],
  tokenize_test "ID then to IDToken" "then"  [Token (TPosition (Point 0 0 0) (Point 0 4 4)) (IDToken "then") ],
  tokenize_test "ID function to IDToken" "function" [Token (TPosition (Point 0 0 0) (Point 0 8 8)) (IDToken "function") ],
  -- Keywords
  tokenize_test "Keyword Where to KToken" "where" [Token (TPosition (Point 0 0 0) (Point 0 5 5)) (KToken Where) ],
  tokenize_test "Keyword Module to KToken" "module" [Token (TPosition (Point 0 0 0) (Point 0 6 6)) (KToken Module) ],
  tokenize_test "Keyword Let to KToken" "let" [Token (TPosition (Point 0 0 0) (Point 0 3 3)) (KToken Let) ],
  tokenize_test "Keyword In to KToken" "in" [Token (TPosition (Point 0 0 0) (Point 0 2 2)) (KToken In) ],
  tokenize_test "Keyword Import to KToken" "import" [Token (TPosition (Point 0 0 0) (Point 0 6 6)) (KToken Import) ],
  tokenize_test "Keyword Backslash to KToken" "\\" [Token (TPosition (Point 0 0 0) (Point 0 1 1)) (KToken Backslash) ],
  tokenize_test "Keyword Data to KToken" "data" [Token (TPosition (Point 0 0 0) (Point 0 4 4)) (KToken Data) ],
  tokenize_test "Keyword Underscore to KToken" "_" [Token (TPosition (Point 0 0 0) (Point 0 1 1)) (KToken Underscore) ],
  tokenize_test "Keyword Class to KToken" "class" [Token (TPosition (Point 0 0 0) (Point 0 5 5)) (KToken Class) ],
  tokenize_test "Keyword Type to KToken" "type" [Token (TPosition (Point 0 0 0) (Point 0 4 4)) (KToken Type) ],
  tokenize_test "Keyword Instance to KToken" "instance" [Token (TPosition (Point 0 0 0) (Point 0 8 8)) (KToken Instance) ],
  -- Puntuation
  tokenize_test "Puntuation LeftParen to a PToken" "(" [Token (TPosition (Point 0 0 0) (Point 0 1 1)) (PToken LeftParen) ],
  tokenize_test "Puntuation RightParen to a PToken" ")" [Token (TPosition (Point 0 0 0) (Point 0 1 1)) (PToken RightParen) ],
  tokenize_test "Puntuation LeftBrace to a PToken" "{" [Token (TPosition (Point 0 0 0) (Point 0 1 1)) (PToken LeftBrace) ],
  tokenize_test "Puntuation RightBrace to a PToken" "}" [Token (TPosition (Point 0 0 0) (Point 0 1 1)) (PToken RightBrace) ],
  tokenize_test "Puntuation SemiColon to a PToken" ";" [Token (TPosition (Point 0 0 0) (Point 0 1 1)) (PToken SemiColon) ],
  tokenize_test "Puntuation Colon to a PToken" ":" [Token (TPosition (Point 0 0 0) (Point 0 1 1)) (PToken Colon) ],
  tokenize_test "Puntuation Equal to a PToken" "=" [Token (TPosition (Point 0 0 0) (Point 0 1 1)) (PToken Equal) ],
  tokenize_test "Puntuation Arrow to a PToken" "=>" [Token (TPosition (Point 0 0 0) (Point 0 2 2)) (PToken Arrow) ],
  tokenize_test "Chaining identifiers and some puntuations" "a=>b:c;d=e" [
    Token (TPosition (Point 0 0 0) (Point 0 1 1)) (IDToken "a"),
    Token (TPosition (Point 0 1 1) (Point 0 3 3)) (PToken Arrow),
    Token (TPosition (Point 0 3 3) (Point 0 4 4)) (IDToken "b"),
    Token (TPosition (Point 0 4 4) (Point 0 5 5)) (PToken Colon),
    Token (TPosition (Point 0 5 5) (Point 0 6 6)) (IDToken "c"),
    Token (TPosition (Point 0 6 6) (Point 0 7 7)) (PToken SemiColon),
    Token (TPosition (Point 0 7 7) (Point 0 8 8)) (IDToken "d"),
    Token (TPosition (Point 0 8 8) (Point 0 9 9)) (PToken Equal),
    Token (TPosition (Point 0 9 9) (Point 0 10 10)) (IDToken "e")
  ],
  -- Ignore comments
  tokenize_test "Ignoring single comment" "--this is a comment" [],
  tokenize_test "Ignoring comment between lines" "a b\n--c d e f\ng"  [
    Token (TPosition (Point 0 0 0) (Point 0 1 1)) (IDToken "a"),
    Token (TPosition (Point 0 2 1) (Point 0 3 2)) (IDToken "b"),
    Token (TPosition (Point 2 0 2) (Point 2 1 3)) (IDToken "g")
  ],
  tokenize_test "Ignoring complex comments" "module Lexer where --where\n--this is a comment\nid a = a --identity" (tokenizeProgram "Test" "module Lexer where\n\nid a = a"),
  -- Complex programs
  tokenize_test "Tokenize a program with two tokens" "42 if" [
    Token (TPosition (Point 0 0 0) (Point 0 2 2)) (NToken 42),
    Token (TPosition (Point 0 3 2) (Point 0 5 4)) (IDToken "if")
  ],
  tokenize_test "Tokenize a program with puntuation and indentifiers" " tokenize (x:xs) = let word = (tokenize x) \n in word : tokenize xs" [
    Token (TPosition (Point 0 1 0) (Point 0 9 8)) (IDToken "tokenize"),
    Token (TPosition (Point 0 10 8) (Point 0 11 9)) (PToken LeftParen),
    Token (TPosition (Point 0 11 9) (Point 0 12 10)) (IDToken "x"),
    Token (TPosition (Point 0 12 10) (Point 0 13 11)) (PToken Colon),
    Token (TPosition (Point 0 13 11) (Point 0 15 13)) (IDToken "xs"),
    Token (TPosition (Point 0 15 13) (Point 0 16 14)) (PToken RightParen),
    Token (TPosition (Point 0 17 14) (Point 0 18 15)) (PToken Equal),
    Token (TPosition (Point 0 19 15) (Point 0 22 18)) (KToken Let),
    Token (TPosition (Point 0 23 18) (Point 0 27 22)) (IDToken "word"),
    Token (TPosition (Point 0 28 22) (Point 0 29 23)) (PToken Equal),
    Token (TPosition (Point 0 30 23) (Point 0 31 24)) (PToken LeftParen),
    Token (TPosition (Point 0 31 24) (Point 0 39 32)) (IDToken "tokenize"),
    Token (TPosition (Point 0 40 32) (Point 0 41 33)) (IDToken "x"),
    Token (TPosition (Point 0 41 33) (Point 0 42 34)) (PToken RightParen),
    Token (TPosition (Point 1 1 34) (Point 1 3 36)) (KToken In),
    Token (TPosition (Point 1 4 36) (Point 1 8 40)) (IDToken "word"),
    Token (TPosition (Point 1 9 40) (Point 1 10 41)) (PToken Colon),
    Token (TPosition (Point 1 11 41) (Point 1 19 49)) (IDToken "tokenize"),
    Token (TPosition (Point 1 20 49) (Point 1 22 51)) (IDToken "xs")
    ]
  ]
