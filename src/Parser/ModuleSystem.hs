module Parser.ModuleSystem(
         Module,
           emptyModule, addSubmodule, declareName,
           exportAllNamesFromModule, exportNames,
         Context,
           emptyContext, 
           importAllNamesFromModule, importNames,
           resolveName
       ) where

import qualified Data.Set as S
import qualified Data.Map as M

import Error(ErrorMessage)
import Lexer.Name(QName(..), qualify)

---- A module represents a tree of namespaces

data ModuleExports = ExportAll
                   | ExportSome (S.Set String)

data Module = Module {
                moduleNames         :: S.Set String,
                moduleExportedNames :: ModuleExports,
                moduleSubmodules    :: M.Map String Module
              }

emptyModule :: Module
emptyModule = Module {
                moduleNames         = S.empty,
                moduleExportedNames = ExportSome S.empty,
                moduleSubmodules    = M.empty
              }

addSubmodule :: QName -> Module -> Either ErrorMessage Module
addSubmodule =
  liftToQName (\ id m ->
    if M.member id (moduleSubmodules m)
     then Left ("Module \"" ++ id ++ "\" already exists.")
     else return (ensureSubmodule m id)
  )

declareName :: QName -> Module -> Either ErrorMessage Module
declareName =
  liftToQName (\ id m ->
     return (m {
       moduleNames = S.insert id (moduleNames m)
     })
  )

exportAllNamesFromModule :: QName -> Module -> Either ErrorMessage Module
exportAllNamesFromModule qname =
  liftToQName (\ _ m -> return (m { moduleExportedNames = ExportAll }))
  (qualify qname "_")

exportSingleName :: QName -> Module -> Either ErrorMessage Module
exportSingleName =
  liftToQName (\ id m ->
    return (m {
      moduleNames = S.insert id (moduleNames m),
      moduleExportedNames =
        let s = (case moduleExportedNames m of
                  ExportAll    -> S.empty
                  ExportSome s -> s)
         in ExportSome (S.insert id s)
    }))

exportNames :: QName -> [String] -> Module -> Either ErrorMessage Module
exportNames moduleName []         m = return m
exportNames moduleName (id : ids) m = do
  m' <- exportSingleName (qualify moduleName id) m
  exportNames moduleName ids m'

--

liftToQName :: (String -> Module -> Either ErrorMessage Module)
            -> QName -> Module -> Either ErrorMessage Module
liftToQName f (Name id) m = f id m
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

moduleExists :: QName -> Module -> Bool
moduleExists (Name id)            m = M.member id (moduleSubmodules m)
moduleExists (Qualified id qname) m =
  M.member id (moduleSubmodules m) &&
  moduleExists qname (M.findWithDefault undefined id (moduleSubmodules m))

nameIsExported :: QName -> Module -> Bool
nameIsExported (Name id) m =
  let e = case moduleExportedNames m of
            ExportAll    -> moduleNames m
            ExportSome s -> s
   in S.member id e
nameIsExported (Qualified id qname) m =
  M.member id (moduleSubmodules m) &&
  nameIsExported qname (M.findWithDefault undefined id (moduleSubmodules m))

---- A context represents a scope in which some modules may or may
---- not be imported

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

importAllNamesFromModule :: QName -> Module -> Context
                         -> Either ErrorMessage Context
importAllNamesFromModule moduleName m c =
  if moduleExists moduleName m
   then
     return (c {
       contextImportedModules = S.insert moduleName (contextImportedModules c)
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

