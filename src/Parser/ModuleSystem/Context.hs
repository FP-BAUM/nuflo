module Parser.ModuleSystem.Context(
         Context,
           emptyContext, contextCurrentModuleName, contextPrecedenceTable,
           importAllNamesFromModule, importNames,
           resolveName
       ) where

import qualified Data.Set as S
import qualified Data.Map as M

import Error(ErrorMessage)
import Syntax.Name(QName(..), qualify)
import Parser.ModuleSystem.Module(Module, moduleExists, nameIsExported)
import Parser.PrecedenceTable(PrecedenceTable)

---- Context ----

---- A context represents a scope in which some modules may or may
---- not be imported

data Context = Context {
                 contextCurrentModuleName :: QName,
                 contextImportedNames     :: M.Map String (S.Set QName),
                 contextImportedModules   :: S.Set QName,
                 contextPrecedenceTable   :: PrecedenceTable
               }

emptyContext :: QName -> Context
emptyContext currentModuleName =
  Context {
    contextCurrentModuleName = currentModuleName,
    contextImportedNames     = M.empty,
    contextImportedModules   = S.fromList [currentModuleName]
    contextPrecedenceTable   = emptyPrecedenceTable
  }

importAllNamesFromModule :: QName -> Module -> Context
                         -> Either ErrorMessage Context
importAllNamesFromModule moduleName m c =
  if moduleExists moduleName m
   then
     return (c {
       contextImportedModules = S.insert moduleName (contextImportedModules c)
       contextPrecedenceTable = S.union (contextPrecedenceTable c) (contextPrecedenceTable m)
     })
   else
     Left ("Module \"" ++ show moduleName ++ "\" does not exist.")

importSingleName :: QName -> String -> Module -> Context
                 -> Either ErrorMessage Context
importSingleName originalName alias m c =
  if nameIsExported originalName m
   then
     let ns = contextImportedNames c in
       return (c {
         contextImportedNames =
           M.insert alias (S.insert originalName
                                    (M.findWithDefault S.empty alias ns))
                          ns
       })
   else
     Left ("Name \"" ++ show originalName ++ "\" is not exported.")

importNames :: QName -> [(String, String)] -> Module -> Context
            -> Either ErrorMessage Context
importNames moduleName []                  _ c = return c
importNames moduleName ((id, alias) : ids) m c = do
  c' <- importSingleName (qualify moduleName id) alias m c
  importNames moduleName ids m c'

resolveName :: Module -> Context -> QName -> Either ErrorMessage QName
resolveName m c (Name id) =
    if null candidates
     then return $ qualify (contextCurrentModuleName c) id
     else
       if null (tail candidates)
        then return $ head candidates
        else Left ("Ambiguous name: \"" ++ id ++ "\". Candidates:\n" ++
                   unlines (map show candidates))
  where
    candidates =
       S.toList (
           -- Explicitly imported name 
           M.findWithDefault S.empty id (contextImportedNames c)
         `S.union`
           -- Implicitly imported name (by importing all from a module)
           S.fromList [ qualifiedName |
              submoduleName <- S.toList (contextImportedModules c),
              qualifiedName <- [qualify submoduleName id],
              nameIsExported qualifiedName m
           ]
       )
resolveName m c qname =
  if nameIsExported qname m
   then return $ qname
   else Left ("Name: \"" ++ show qname ++ "\" does not exist.")

