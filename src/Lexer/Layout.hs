module Lexer.Layout(layout) where

import Position(Position(..), unknownPosition)
import Error(Error(..), ErrorType(..))
import Lexer.Token(Token(..), TokenType(..))

layout :: [Token] -> Either Error [Token]
layout tokens = lay (prepare tokens) []

---- Implementation of the layout algorithm,
---- following Haskell '98 standard (sec. 9.3).

data LToken = T Token
            | A Integer  -- <n>
            | B Integer  -- {n}
  deriving Show --TODO:BORRAR

line :: Token -> Integer
line t = positionLine (tokenStartPos t)

column :: Token -> Integer
column t = positionColumn (tokenStartPos t)

isBeginModuleKeyword :: Token -> Bool
isBeginModuleKeyword t =
  case tokenType t of
    T_Module -> True
    T_LBrace -> True
    _        -> False

isLayoutKeyword :: Token -> Bool
isLayoutKeyword t =
  case tokenType t of
    T_Where -> True
    T_Let   -> True
    _       -> False

isLBrace :: Token -> Bool
isLBrace t =
  case tokenType t of
    T_LBrace -> True
    _        -> False

isRBrace :: Token -> Bool
isRBrace t =
  case tokenType t of
    T_RBrace -> True
    _        -> False

startsWithLBrace :: [Token] -> Bool
startsWithLBrace []      = False
startsWithLBrace (t : _) = isLBrace t

-- Prepare input for layout algorithm

prepare :: [Token] -> [LToken]
prepare []         = []
prepare ts@(t : _) =
    if isBeginModuleKeyword t
     then rec False 0 ts
     else B (column t) : rec True 0 ts
  where
    rec :: Bool     -- Has {n} been inserted right before?
        -> Integer  -- Line of previous token 
        -> [Token]
        -> [LToken]
    rec _ _ [] = []
    rec b l (t : ts)
      | isLayoutKeyword t && not (startsWithLBrace ts) =
         continue b l t ++ T t : B n : rec True (line t) ts
      where n = if null ts
                 then 0
                 else column (head ts)
    rec b l (t : ts) =
         continue b l t ++ T t : rec False (line t) ts

    continue False l t | line t > l = [A (column t)]
    continue _     _ _              = []

-- Layout algorithm

newToken :: [LToken] -> TokenType -> Token
newToken []         x = Token unknownPosition unknownPosition x
newToken (A _ : ts) x = newToken ts x
newToken (B _ : ts) x = newToken ts x 
newToken (T t : ts) x = Token (tokenStartPos t) (tokenEndPos t) x 

lay :: [LToken] -> [Integer] -> Either Error [Token]
lay (A n : ts) (m : ms)
  | m == n               = do ls <- lay ts (m : ms)
                              return (newToken ts T_Semicolon : ls)
  | n < m                = do ls <- lay (A n : ts) ms
                              return (newToken ts T_RBrace : ls)
lay (A _ : ts) ms        = lay ts ms
lay (B n : ts) (m : ms)
  | n > m                = do ls <- lay ts (n : m : ms)
                              return (newToken ts T_LBrace : ls)
lay (B n : ts) []
  | n > 0                = do ls <- lay ts [n]
                              return (newToken ts T_LBrace : ls)
lay (B n : ts) ms        = do ls <- lay (A n : ts) ms
                              return (newToken ts T_LBrace :
                                      newToken ts T_RBrace : ls)
lay (T t : ts) (0 : ms)
  | isRBrace t           = do ls <- lay ts ms
                              return (t : ls)
lay (T t : ts) ms 
  | isRBrace t           = Left (Error LexerErrorLayout (tokenStartPos t)
                                       "Explicit close brace matches \
                                       \implicit open brace.")
lay (T t : ts) ms
  | isLBrace t           = do ls <- lay ts (0 : ms)
                              return (newToken ts T_LBrace : ls)
lay (T t : ts) ms        = do ls <- lay ts ms
                              return (t : ls)
lay []         []        = return []
lay []         (m : ms)
  | m > 0                = do ls <- lay [] ms
                              return (newToken [] T_RBrace : ls)
lay _          _         = Left (Error LexerErrorLayout unknownPosition
                                         "Layout error.")

