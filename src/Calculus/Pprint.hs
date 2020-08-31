module Calculus.Pprint(pprintInNamespace) where

import Debug.Trace(trace)

import Syntax.Name(QName(..), nameArity, splitParts, primitiveOk)
import Calculus.Terms(Term(..), splitArgs)
import ModuleSystem.Namespace(
         Namespace, unresolveQName,
         Associativity(..), Precedence(..),
         associativityAndPrecedence
       )

pprintInNamespace :: Namespace -> Term -> String
pprintInNamespace ns t = pp ns t

pp :: Namespace -> Term -> String
-- Values
pp ns (Var x)       = ppQName ns x
pp ns (Cons c)      = ppQName ns c
pp ns (ConstInt n)  = show n
pp ns (ConstChar c) = show c
pp ns (LamL l _ _)  = "{closure}"
pp ns t@(App _ _)   =
  let (f, xs) = splitArgs t in
    case showOpAsMixfix ns f xs of
      Just res -> res
      Nothing  -> unwords (pp ns f : map (ppParen ns) xs)
-- Stuck terms
pp ns _            = "{stuck}"

ppQName :: Namespace -> QName -> String
ppQName ns qname
  | qname == primitiveOk = "()"
  | otherwise            = show (unresolveQName ns qname)

parenthesize :: String -> String
parenthesize x = "(" ++ x ++ ")"

ppParen :: Namespace -> Term -> String
ppParen ns t@(App _ _) = parenthesize (pp ns t)
ppParen ns t           = pp ns t

showOpAsMixfix :: Namespace -> Term -> [Term] -> Maybe String
showOpAsMixfix ns (Var f)  args = showNameAsMixfix ns f args
showOpAsMixfix ns (Cons f) args = showNameAsMixfix ns f args
showOpAsMixfix _ _ _ = Nothing

showNameAsMixfix :: Namespace -> QName -> [Term] -> Maybe String
showNameAsMixfix ns f args =
  case unresolveQName ns f of
    Name operator -> do
      (associativity, precedence) <- associativityAndPrecedence ns f
      let arity = nameArity operator 
      if arity <= length args
       then let (actualArgs, remainingArgs) = splitAt arity args
                actualArgs'    = mixfixParenthesizedArgs
                                   ns associativity precedence actualArgs
                remainingArgs' = map (ppParen ns) remainingArgs
                result = mixfixIntersperse operator actualArgs'
             in if null remainingArgs
                 then return result
                 else return $ unwords (parenthesize result : remainingArgs')
       else Nothing
    _ -> Nothing

mixfixIntersperse :: String -> [String] -> String
mixfixIntersperse operator args = rec (splitParts operator) args
  where
    rec []         _        = error "Empty operator."
    rec ["_"]      [x]      = x
    rec [part]     []       = part
    rec ("_" : ps) (x : xs) = x ++ " " ++ rec ps xs
    rec (p : ps)   xs       = p ++ " " ++ rec ps xs

mixfixParenthesizedArgs :: Namespace -> Associativity -> Precedence
                        -> [Term] -> [String]
mixfixParenthesizedArgs ns associativity precedence args =
  let thresholds = parenthesizationThresholds
                     (length args) associativity precedence
   in zipWith (\ arg threshold ->
                let arg'          = pp ns arg
                    maxPrecedence = precedence + 2
                 in if precedenceLevel ns arg maxPrecedence < threshold
                     then parenthesize arg'
                     else arg')
              args
              thresholds

-- Calculate the list of parenthesization thresholds for the n arguments
-- of an n-ary mixfix operator with the given precedences and associativities.
--
-- The thresholds for an operator of precedence p are:
--   - Left associative:    [p, p + 1, ..., p + 1, p + 1]
--   - Right associative:   [p + 1, p + 1, ..., p + 1, p]
--   - Non-associative:     [p + 1, p + 1, ..., p + 1, p + 1]
--
-- Arguments whose precedence is strictly less than the threshold
-- must be parenthesized.
parenthesizationThresholds :: Int -> Associativity -> Precedence
                           -> [Precedence]
parenthesizationThresholds n LeftAssoc  p = p : replicate (n - 1) (p + 1)
parenthesizationThresholds n RightAssoc p = replicate (n - 1) (p + 1) ++ [p]
parenthesizationThresholds n NonAssoc   p = replicate n (p + 1)

----

-- Calculate the precedence level of a term.
--
-- If t is headed by a mixfix operator
-- taking *exactly* the number of arguments
-- as the number of holes it has, then
--   precedenceLevel ns t mx = <precedence level of the mixfix operator>
-- otherwise,
--   precedenceLevel ns t mx = mx
--
precedenceLevel :: Namespace -> Term -> Precedence -> Precedence
-- Values
precedenceLevel ns (Var x)       mx = mx
precedenceLevel ns (Cons c)      mx = mx
precedenceLevel ns (ConstInt _)  mx = mx
precedenceLevel ns (ConstChar _) mx = mx
precedenceLevel ns (LamL l _ _)  mx = mx
precedenceLevel ns t@(App _ _)   mx =
  let (f, xs) = splitArgs t in
    case mixfixOpPrecedenceLevel ns f xs of
      Just res -> res
      Nothing  -> mx
-- Stuck terms
precedenceLevel ns _             mx = mx

mixfixOpPrecedenceLevel :: Namespace -> Term -> [Term] -> Maybe Precedence
mixfixOpPrecedenceLevel ns (Var f)  args = mixfixNamePrecedenceLevel ns f args
mixfixOpPrecedenceLevel ns (Cons f) args = mixfixNamePrecedenceLevel ns f args
mixfixOpPrecedenceLevel _ _ _ = Nothing

mixfixNamePrecedenceLevel :: Namespace -> QName -> [Term] -> Maybe Precedence
mixfixNamePrecedenceLevel ns f args =
  case unresolveQName ns f of
    Name operator -> do
      (_, precedence) <- associativityAndPrecedence ns f
      if nameArity operator == length args
       then return precedence
       else Nothing
    _ -> Nothing

