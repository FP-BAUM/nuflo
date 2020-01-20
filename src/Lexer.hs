module Lexer (
  Token(..),
  TokenType(..),
  Puntuation(..),
  KeyWord(..),
  TPosition(..),
  Point(..),
  tokenizeProgram,
) where

import Utils
import Point
import TContext
import Error

data Token = Token { position :: TPosition , tokenType :: TokenType } deriving (Eq, Show)

data TPosition = TPosition { start :: Point, end :: Point } deriving (Eq, Show)

data TokenType =  PToken Puntuation |
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

------------------------------

positionFromContext :: TContext -> TContext -> TPosition
positionFromContext context nextContext = TPosition (pointFromContext context) (pointFromContext nextContext)

token :: TContext -> TContext -> TokenType -> Token
token context nextContext tokenType = Token (positionFromContext context nextContext) tokenType

consumeWord :: TContext -> String -> Either Error (String, TContext)
consumeWord c ""                                 = Right ("", c)
consumeWord c ('=':'>':xs)                       = Right ("=>", incrementColumn 2 c)
consumeWord c w@(x:xs) | isPuntuation (x:"")     = Right (x:"", incrementColumn 1 c)
                       | otherwise               =  let word = nextWord w
                                                    in checkWordlexicographically c w (word, incrementColumn (toInteger (length word)) c)

checkWordlexicographically :: TContext -> String -> (String, TContext) -> Either Error (String, TContext)
checkWordlexicographically c s tuple@(word, _)  | validWord word = Right tuple
                                                | otherwise      = Left (Error LexicographicalError (word ++ " is not a valid word") (pointFromContext c) s) 

validWord :: String -> Bool
validWord "__" = False
validWord _    = True
------------------------------

tokenizeProgram :: String -> String -> Either Error [Token]
tokenizeProgram fileName source = offsideRules $ tokenizeProgramWithContext (context fileName source) source

offsideRules :: Either Error [Token] -> Either Error [Token]
offsideRules err@(Left _) = err
offsideRules (Right ts) = Right (applyOffsideRules ts [])

-- Note1 = < n >
-- Note2 = { n }
applyOffsideRules :: [Token] -> [Int] -> [Token]
-- Case < n > TODO: FIX THE POINTS
applyOffsideRules ((OffsideToken Note1 n):ts) (m:ms)  | m == n    = (Token (TPosition (Point 0 0 0) (Point 0 0 0) }) SemiColon) : (applyOffsideRules  ts (m:ms))
                                                      | n < m     = (Token (TPosition (Point 0 0 0) (Point 0 0 0) }) RightBrace) : (applyOffsideRules ((OffsideToken Note1 n):ts) ms)
                                                      | otherwise = applyOffsideRules ts (m:ms)
-- Case { n } TODO: FIX THE POINTS
applyOffsideRules ((OffsideToken Note2 n) :ts) []                 = (Token (TPosition (Point 0 0 0) (Point 0 0 0) }) LeftBrace) : (applyOffsideRules ts [n])
applyOffsideRules ((OffsideToken Note2 n) :ts) (m:ms) | n > m     = (Token (TPosition (Point 0 0 0) (Point 0 0 0) }) LeftBrace) : (applyOffsideRules ts (n:m:ms))
                                                      | otherwise = (Token (TPosition (Point 0 0 0) (Point 0 0 0) }) LeftBrace) : (Token (TPosition (Point 0 0 0) (Point 0 0 0) }) RightBrace) : (applyOffsideRules ((OffsideToken Note1 n):ts) (m:ms))
-- Case } TODO: FIX THE POINTS
applyOffsideRules ((Token { position = pos, tokenType = (KToken LeftBrace)}):ts) (0:ms) = (Token (TPosition (Point 0 0 0) (Point 0 0 0) }) RightBrace) : (applyOffsideRules ts ms)
applyOffsideRules ((Token { position = pos, tokenType = (KToken LeftBrace)}):ts) ms     = [] -- Here is parse error

-- Case { TODO: FIX THE POINTS
applyOffsideRules ((Token { position = pos, tokenType = (KToken LeftBrace)}):ts) ms = (Token (TPosition (Point 0 0 0) (Point 0 0 0) }) LeftBrace): applyOffsideRules ts (0:ms)

-- General cases t:ts TODO: FIX THE POINTS
applyOffsideRules (t:ts) (m:ms)                       | m != 0    = (Token (TPosition (Point 0 0 0) (Point 0 0 0) }) RightBrace) : (applyOffsideRules (t:ts) ms)
                                                      | otherwise = t : (applyOffsideRules ts ms)
-- Bases cases [] TODO: FIX THE POINTS
applyOffsideRules [] []                               = []
applyOffsideRules [] (m:ms)                           | m != 0    = (Token (TPosition (Point 0 0 0) (Point 0 0 0) }) RightBrace) : (applyOffsideRules [] ms)
                                                      | otherwise = [] -- parse error


tokenizeProgramWithContext :: TContext -> String -> Either Error [Token]
tokenizeProgramWithContext context ""           = Right []
tokenizeProgramWithContext context (' ':xs)     = tokenizeProgramWithContext (incrementColumn 1 context) xs
tokenizeProgramWithContext context ('\n':xs)    = tokenizeProgramWithContext (incrementRow context) xs
tokenizeProgramWithContext context ('-':'-':xs) = tokenizeProgramWithContext (incrementRow context) (removeLine xs)
tokenizeProgramWithContext context source       =
  case consumeWord context source of  Right (word, context') -> let l = toInteger (length word)
                                                                in tokenizeWordWithContext word context (incrementIndex l context') source
                                      Left error -> Left error
                                                                                     

tokenizeWordWithContext :: String -> TContext -> TContext -> String -> Either Error [Token]
tokenizeWordWithContext word previousContext nextContext source =
  case tokenizeProgramWithContext nextContext (drop (length word) source) of Right recursiveResult -> let t = if isKeyword word
                                                                                                              then token previousContext nextContext (KToken (tokenizeKeyword word))
                                                                                                              else  if isPuntuation word
                                                                                                                    then token previousContext nextContext (PToken (tokenizePuntuation word))
                                                                                                                    else  if isNumeric word
                                                                                                                          then token previousContext nextContext (NToken (read word :: Int))
                                                                                                                          else token previousContext nextContext (IDToken word)
                                                                                                      in Right (t : recursiveResult)
                                                                             error -> error

tokenizeKeyword :: String -> KeyWord
tokenizeKeyword "where"    = Where
tokenizeKeyword "module"   = Module
tokenizeKeyword "let"      = Let
tokenizeKeyword "in"       = In
tokenizeKeyword "import"   = Import
tokenizeKeyword "\\"       = Backslash
tokenizeKeyword "data"     = Data
tokenizeKeyword "_"        = Underscore
tokenizeKeyword "class"    = Class
tokenizeKeyword "type"     = Type
tokenizeKeyword "instance" = Instance

tokenizePuntuation :: String -> Puntuation
tokenizePuntuation "("  = LeftParen
tokenizePuntuation ")"  = RightParen
tokenizePuntuation "{"  = LeftBrace
tokenizePuntuation "}"  = RightBrace
tokenizePuntuation ";"  = SemiColon
tokenizePuntuation ":"  = Colon
tokenizePuntuation "="  = Equal
tokenizePuntuation "=>" = Arrow
