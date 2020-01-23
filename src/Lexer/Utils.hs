module Lexer.Utils (
  isNumeric,
  isKeyword,
  isPuntuation,
  removeLine,
  nextWord
) where

keywords = ["where", "let", "in", "import", "\\", "data", "_", "class", "type", "instance", "module"]

puntuations = ["(", ")", "{", "}", ";", ":", "=", "=>"]

isInteger :: String -> Bool
isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False

isDouble :: String -> Bool
isDouble s = case reads s :: [(Double, String)] of
  [(_, "")] -> True
  _         -> False
  
isNumeric :: String -> Bool
isNumeric s = isInteger s || isDouble s

isKeyword :: String -> Bool
isKeyword = flip elem keywords

isPuntuation :: String -> Bool
isPuntuation = flip elem puntuations

removeLine :: String -> String
removeLine "" = ""
removeLine ('\n':xs) = xs
removeLine (_:xs) = removeLine xs

nextWord :: String -> String
nextWord ""                                                      = ""
nextWord ('-':'-':xs)                                            = ""
nextWord (' ':xs)                                                = ""
nextWord ('\n':xs)                                               = "" 
nextWord (x:xs) | isPuntuation (x:"")                            = ""
                | otherwise                                      = x : nextWord xs
