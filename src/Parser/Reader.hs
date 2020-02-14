
module Parser.Reader(readSource) where

-- Read and tokenize a file, including its dependencies.

import qualified Data.Set as S

import Error(Error, ErrorType(..), errorAtUnknown)
import Lexer.Token(Token(..), TokenType(..), isId, isDot)
import Lexer.Name(QName(..), readName)
import Lexer.Lexer(tokenize)

readSource :: FilePath -> IO (Either Error [Token])
readSource filename = do
  result <- readDep S.empty S.empty filename
  case result of
    Left  e         -> return $ Left e
    Right (_, toks) -> return $ Right toks

type Files = S.Set FilePath

readDep :: Files -> Files -> FilePath -> IO (Either Error (Files, [Token]))
readDep codependencies visited filename = do
  if S.member filename codependencies
   then return $ Left (errorAtUnknown
                         ReaderErrorCyclicDependencies
                         ("Cyclic dependencies: " ++ show codependencies))
   else
     if S.member filename visited
      then return $ Right (visited, [])
      else do
        source <- readFile filename
        case tokenize filename source of
          Left  e            -> return $ Left e
          Right moduleTokens ->
            let deps = collectDependencies moduleTokens in do
             res <- readDeps (codependencies `S.union` S.fromList [filename])
                             (visited `S.union` S.fromList [filename])
                             (map qnameToFilename deps)
             case res of
               Left e                      -> return $ Left e
               Right (visited', depTokens) ->
                 return $ Right (visited', depTokens ++ moduleTokens)

readDeps :: Files -> Files -> [FilePath] -> IO (Either Error (Files, [Token]))
readDeps codependencies visited0 [] =
  return $ Right (visited0, [])
readDeps codependencies visited0 (f : fs) = do
  res1 <- readDep codependencies visited0 f
  case res1 of
    Left e                  -> return $ Left e
    Right (visited1, toks1) -> do
      res2 <- readDep codependencies visited1 f
      case res2 of
        Left e                  -> return $ Left e
        Right (visited2, toks2) -> return $ Right (visited2, toks1 ++ toks2)

qnameToFilename :: QName -> FilePath
qnameToFilename (Name parts)       = concat parts ++ ".la"
qnameToFilename (Qualified q name) = q ++ "/" ++ qnameToFilename name

collectDependencies :: [Token] -> [QName]
collectDependencies []                        = []
collectDependencies (Token _ _ T_Import : ts) =
  case readQName ts of
    Just (qname, ts') -> qname : collectDependencies ts'
    Nothing           -> collectDependencies ts
collectDependencies (_ : ts) = collectDependencies ts

readQName :: [Token] -> Maybe (QName, [Token])
readQName (Token _ _ (T_Id s) : Token _ _ T_Dot : t : rest)
  | isId t    = do (q, rest') <- readQName (t : rest)
                   return (Qualified s q, rest')
readQName (Token _ _ (T_Id s) : ts)
  | null ts || not (isDot (head ts))
              = return (readName s, ts)
readQName _   = Nothing

