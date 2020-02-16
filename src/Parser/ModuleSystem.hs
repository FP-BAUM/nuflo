module Parser.ModuleSystem(
         Module, emptyModule, addSubmodule, declareName,
         Context, importAllNamesFromModule, importSingleName, resolveName
       ) where

import qualified Data.Set as S
import qualified Data.Map as M

import Error(ErrorMessage)
import Lexer.Name(QName(..), qualify)

---- A module represents a tree of namespaces.

data Module = Module {
                moduleNames         :: S.Set String,
                moduleExportedNames :: S.Set String,
                moduleSubmodules    :: M.Map String Module
              }
  deriving Show

emptyModule :: Module
emptyModule = Module {
                moduleNames         = S.empty,
                moduleExportedNames = S.empty,
                moduleSubmodules    = M.empty
              }

addSubmodule :: QName -> Module -> Either ErrorMessage Module
addSubmodule =
  liftToQName (\ parts m ->
    let id = concat parts in
      if M.member id (moduleSubmodules m)
       then Left ("Module \"" ++ id ++ "\" already exists.")
       else return (ensureSubmodule m id)
  )

declareName :: QName -> Module -> Either ErrorMessage Module
declareName =
  liftToQName (\ parts m ->
    let id = concat parts in
      if S.member id (moduleNames m)
       then Left ("Name \"" ++ concat parts ++ "\" already declared.")
       else return (m {
              moduleNames = S.insert id (moduleNames m)
            })
  )

exportName :: QName -> Module -> Either ErrorMessage Module
exportName =
  liftToQName (\ parts m ->
    let id = concat parts in
      if not (S.member id (moduleNames m))
       then Left ("Cannot export undeclared name \"" ++ concat parts ++ "\".")
       else return (m {
              moduleExportedNames = S.insert id (moduleExportedNames m)
            })
  )

--

liftToQName :: ([String] -> Module -> Either ErrorMessage Module)
            -> QName -> Module -> Either ErrorMessage Module
liftToQName f (Name parts) m = f parts m
liftToQName f (Qualified id qname) m =
  let m'   = ensureSubmodule m id
      sub  = M.findWithDefault undefined id (moduleSubmodules m')
   in do
        sub' <- liftToQName f qname sub
        return (m' {
                  moduleSubmodules = M.insert id sub' (moduleSubmodules m')
                })

ensureSubmodule :: Module -> String -> Module
ensureSubmodule m id
  | M.member id (moduleSubmodules m) = m
  | otherwise =
      m {
        moduleSubmodules = M.insert id emptyModule (moduleSubmodules m)
      }

nameExists :: QName -> Module -> Bool
nameExists (Name parts) m = S.member (concat parts) (moduleNames m)
nameExists (Qualified id qname) m =
  M.member id (moduleSubmodules m) &&
  nameExists qname (M.findWithDefault undefined id (moduleSubmodules m))

---- A context represents a scope in which some modules may or may
---- not be imported.

data Context = Context {
                 contextCurrentModuleName :: QName,
                 contextImportedNames     :: M.Map String (S.Set QName),
                 contextImportedModules   :: S.Set QName
               }

emptyContext :: QName -> Context
emptyContext currentModuleName =
  Context {
    contextCurrentModuleName = currentModuleName,
    contextImportedNames     = M.empty,
    contextImportedModules   = S.fromList [currentModuleName]
  }

importAllNamesFromModule :: QName -> Context -> Context
importAllNamesFromModule moduleName c =
  c {
    contextImportedModules = S.insert moduleName (contextImportedModules c)
  }

importSingleName :: QName -> String -> Context -> Context
importSingleName originalName alias c =
  let ns = contextImportedNames c in
    c {
      contextImportedNames =
        M.insert alias (S.insert originalName
                                 (M.findWithDefault S.empty alias ns))
                       ns
    }

resolveName :: Module -> Context -> QName -> Either ErrorMessage QName
resolveName m c (Name parts) =
    if null candidates
     then return $ qualify (contextCurrentModuleName c) id
     else
       if null (tail candidates)
        then return $ head candidates
        else Left ("Ambiguous name: \"" ++ id ++ "\". Candidates:\n" ++
                   unlines (map show candidates))
  where
    id = concat parts
    candidates =
       S.toList (
           -- Explicitly imported name 
           M.findWithDefault S.empty id (contextImportedNames c)
         `S.union`
           -- Implicitly imported name (by importing all from a module)
           S.fromList [ qualifiedName |
              submoduleName <- S.toList (contextImportedModules c),
              qualifiedName <- [qualify submoduleName id],
              nameExists qualifiedName m
           ]
       )
resolveName m c qname =
  if nameExists qname m
   then return $ qname
   else Left ("Name: \"" ++ show qname ++ "\" does not exist.")

