module Lexer.OffsideRules (
  offsideRules,
  applyOffsideRules,
) where

import Lexer.Utils
import Lexer.Point
import Lexer.Token
import Lexer.TContext
import Lexer.Error

startPointFromPosition :: TPosition -> Point
startPointFromPosition TPosition { start = s, end = e } = s

offsideRules :: Either Error [Token] -> Either Error [Token]
offsideRules err@(Left _) = err
offsideRules (Right ts) = applyOffsideRules ts []

-- Note1 = < n >
-- Note2 = { n }
applyOffsideRules :: [Token] -> [Int] -> Either Error [Token]
-- Case < n > TODO: FIX THE POINTS
applyOffsideRules ((OffsideToken Note1 n):ts) (m:ms)  | m == n    = case (applyOffsideRules  ts (m:ms)) of
                                                                                                        Right ts -> Right((Token (TPosition emptyPoint emptyPoint) (PToken SemiColon)) : ts)
                                                                                                        Left error -> Left error
                                                      | n < m     = case (applyOffsideRules ((OffsideToken Note1 n):ts) ms) of
                                                                                                                            Right ts -> Right ((Token (TPosition emptyPoint emptyPoint) (PToken RightBrace)) : ts)
                                                                                                                            Left error -> Left error
                                                      | otherwise = applyOffsideRules ts (m:ms)
-- Case { n } TODO: FIX THE POINTS
applyOffsideRules ((OffsideToken Note2 n) :ts) []                 = case (applyOffsideRules ts [n]) of
                                                                                                    Right tokens -> Right ((Token (TPosition emptyPoint emptyPoint) (PToken LeftBrace)) : tokens)
                                                                                                    Left error   -> Left error
applyOffsideRules ((OffsideToken Note2 n) :ts) (m:ms) | n > m     = case (applyOffsideRules ts (n:m:ms))  of
                                                                                                          Right tokens -> Right ((Token (TPosition emptyPoint emptyPoint) (PToken LeftBrace)) : tokens)
                                                                                                          Left error   -> Left error 
                                                      | otherwise = case (applyOffsideRules ((OffsideToken Note1 n):ts) (m:ms)) of
                                                                                                                                Right tokens -> Right ((Token (TPosition emptyPoint emptyPoint) (PToken LeftBrace)) : (Token (TPosition emptyPoint emptyPoint) (PToken RightBrace)) : tokens)
                                                                                                                                Left error   -> Left error
-- Case } TODO: FIX THE POINTS
applyOffsideRules ((Token { position = pos, tokenType = (PToken LeftBrace)}):ts) (0:ms) = case (applyOffsideRules ts ms)  of
                                                                                                                          Right tokens -> Right ((Token (TPosition emptyPoint emptyPoint) (PToken RightBrace)) : tokens)
                                                                                                                          Left error   -> Left error
applyOffsideRules ((Token { position = pos, tokenType = (PToken LeftBrace)}):ts) ms     = Left (Error LexicographicalError ("offside rules error") (startPointFromPosition pos) "{")

-- Case { TODO: FIX THE POINTS
applyOffsideRules ((Token { position = pos, tokenType = (PToken LeftBrace)}):ts) ms = case applyOffsideRules ts (0:ms)  of
                                                                                                                        Right tokens -> Right ((Token (TPosition emptyPoint emptyPoint) (PToken LeftBrace)) : tokens)
                                                                                                                        Left error   -> Left error

-- General cases t:ts TODO: FIX THE POINTS
applyOffsideRules (t:ts) (m:ms)                       | m /= 0    = case (applyOffsideRules (t:ts) ms)  of
                                                                                                        Right tokens -> Right ((Token (TPosition emptyPoint emptyPoint) (PToken RightBrace)) : tokens)
                                                                                                        Left error   -> Left error

                                                      | otherwise = Left (Error LexicographicalError ("offside rules error") (startPointFromPosition (TPosition emptyPoint emptyPoint)) "")

applyOffsideRules (t:ts) ms                                       = case (applyOffsideRules ts ms)  of
                                                                                                    Right tokens -> Right (t : tokens)
                                                                                                    Left error   -> Left error
-- Bases cases [] TODO: FIX THE POINTS
applyOffsideRules [] []                               = Right []
applyOffsideRules [] (m:ms)                           | m /= 0    = case (applyOffsideRules [] ms)  of
                                                                                                    Right tokens -> Right ((Token (TPosition emptyPoint emptyPoint) (PToken RightBrace)) : tokens)
                                                                                                    Left error   -> Left error
                                                      | otherwise = Left (Error LexicographicalError ("offside rules error") (startPointFromPosition (TPosition emptyPoint emptyPoint)) "")
