
module TestLexer(tests) where

import Test(TestSuite(..), Test(..))

import Lexer

tokenize_test :: String -> String -> [[Token]] -> Test
tokenize_test description source expectedTokens =
  TestCase description expectedTokens (tokenizeProgram source)

tests :: TestSuite
tests = TestSuite "Lexer" [
  tokenize_test "Empty program" "" [],
  -- Numbers
  tokenize_test "number" "42" [[NToken 42]],
  tokenize_test "number" "01" [[NToken 01]],
  tokenize_test "number" "10" [[NToken 10]],
  -- Identifiers
  tokenize_test "if" "if" [[IDToken "if"]],
  tokenize_test "then" "then" [[IDToken "then"]],
  tokenize_test "function" "function" [[IDToken "function"]],
  -- Keywords
  tokenize_test "Where" "where" [[KToken Where]],
  tokenize_test "Module" "module" [[KToken Module]],
  tokenize_test "Let" "let" [[KToken Let]],
  tokenize_test "In" "in" [[KToken In]],
  tokenize_test "Import" "import" [[KToken Import]],
  tokenize_test "Backslash" "\\" [[KToken Backslash]],
  tokenize_test "Data" "data" [[KToken Data]],
  tokenize_test "Underscore" "_" [[KToken Underscore]],
  tokenize_test "Class" "class" [[KToken Class]],
  tokenize_test "Type" "type" [[KToken Type]],
  tokenize_test "Instance" "instance" [[KToken Instance]],
  -- Puntuation
  tokenize_test "LeftParen" "(" [[PToken LeftParen]],
  tokenize_test "RightParen" ")" [[PToken RightParen]],
  tokenize_test "LeftBrace" "{" [[PToken LeftBrace]],
  tokenize_test "RightBrace" "}" [[PToken RightBrace]],
  tokenize_test "SemiColon" ";" [[PToken SemiColon]],
  tokenize_test "Colon" ":" [[PToken Colon]],
  tokenize_test "Equal" "=" [[PToken Equal]],
  tokenize_test "Arrow" "=>" [[PToken Arrow]],
  -- Ignore comments
  tokenize_test "Comment" "--this is a comment" [],
  tokenize_test "Comment between lines" "a b\n--c d e f\ng" [[IDToken "a", IDToken "b"], [IDToken "g"]],
  tokenize_test "Comments" "module Lexer where --where\n--this is a comment\nid a = a --identity" (tokenizeProgram "module Lexer where\nid a = a"),
  -- Complex programs
  tokenize_test "numbers and identifiers" "42 if \n then else def 44" [[NToken 42, IDToken "if"], [IDToken "then", IDToken "else", IDToken "def", NToken 44]]
  ]
