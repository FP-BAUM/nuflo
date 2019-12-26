import Test.HUnit

import Lexer

tokenize_test :: String -> String -> [[Token]] -> Test
tokenize_test description program tokens = TestCase (assertEqual description (tokenizeProgram program)  tokens)

tests = TestList [
  tokenize_test "Empty program" "" [[]],
  tokenize_test "Only a number" "42" [[NToken 42]]]
