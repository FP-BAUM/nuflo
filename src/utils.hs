module Utils ( isNumeric, isKeyword, isPuntuation ) where

keywords = ["where", "let", "in", "import", "\\", "data", "_", "class", "type", "instance"]

puntuations = ["(", ")", "{", "}", ";", ":", "=", "=>"]

isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False
  
isDouble s = case reads s :: [(Double, String)] of
  [(_, "")] -> True
  _         -> False
  
isNumeric :: String -> Bool
isNumeric s = isInteger s || isDouble s

isKeyword :: String -> Bool
isKeyword = flip elem keywords

isPuntuation :: String -> Bool
isPuntuation = flip elem puntuations
