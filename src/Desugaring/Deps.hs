{-# LANGUAGE ScopedTypeVariables #-}

module Desugaring.Deps(
        Dependency(..), dependencySortL,
        Graph, graphFromList, topoSort, transpose,
        -- Only for unit testing:
        dfs, stronglyConnectedComponents,
    ) where

import Control.Monad.Trans.State.Lazy(State, get, put, modify, evalState)
import qualified Data.Map as Map(
        Map, empty, insert, lookup, findWithDefault, keys, fromList,
        delete, map
    )
import qualified Data.Set as S
import Data.List(union, intersect, (\\), nub)
import qualified Calculus.Terms as C
import Syntax.Name(QName(..))

type Graph a = Map.Map a [a]

graphFromList :: Ord a => [(a, [a])] -> Graph a
graphFromList = Map.fromList

type DepGraph = Graph IdL

dfs :: forall a. Ord a => Graph a -> a -> [a]
dfs graph node = evalState (rec node) Map.empty
  where
    rec :: Ord a => a -> State (Map.Map a ()) [a]
    rec v = do
      visited <- get
      case Map.lookup v visited of
        Just _  -> return []
        Nothing -> do
          modify (Map.insert v ())
          vs <- mapM rec (Map.findWithDefault [] v graph)
          return (v : concat (reverse vs))

transpose :: forall a. Ord a => Graph a -> Graph a
transpose graph =
    Map.fromList $ map (\ v -> (v, sources v))
                       (Map.keys graph)
  where
    sources :: Ord a => a -> [a]
    sources v = filter (\ w -> v `elem` Map.findWithDefault [] w graph)
                       (Map.keys graph)

removeNode :: Ord a => a -> Graph a -> Graph a
removeNode v graph =
  Map.map (\ adj -> adj \\ [v]) $ Map.delete v graph

-- Should be a DAG

type TopoState a = (Map.Map a (), Map.Map a ())

topoSort :: forall a. Ord a => Graph a -> [a]
topoSort graph =
    evalState ts (Map.empty, Map.empty)
  where
    ts :: Ord a => State (TopoState a) [a]
    ts = do
      vs <- mapM visitUnmarked (Map.keys graph)
      return $ concat (reverse vs)
    visitUnmarked :: Ord a => a -> State (TopoState a) [a]
    visitUnmarked v = do
      (_, marked) <- get
      case Map.lookup v marked of
        Nothing -> visit v
        Just _  -> return []
    visit :: Ord a => a -> State (TopoState a) [a]
    visit v = do
      (visited, marked) <- get
      case Map.lookup v marked of
        Nothing -> return []
        Just _  -> error "Already visited: not a DAG."
      case Map.lookup v visited of
        Just _  -> return []
        Nothing -> do
          modify (\ (vv, mm) -> (vv, Map.insert v () mm))
          vs <- mapM visit (Map.findWithDefault [] v graph)
          modify (\ (vv, mm) -> (Map.insert v () vv, Map.delete v mm))
          return (v : concat (reverse vs))

unionMap :: Eq b => (a -> [b]) -> [a] -> [b]
unionMap f = foldr union [] . map f

type ExprL = C.Term

varsL :: ExprL -> [IdL]
varsL = S.toList . C.freeVariables

type DeclarationL = (QName, C.Term)

type IdL = QName

dependencyGraphL :: [DeclarationL] -> DepGraph
dependencyGraphL decls =
    Map.fromList $
      map (\ v -> (v, deps v [] decls)) nodes
  where
    nodes :: [IdL]
    nodes = map var decls

    var :: DeclarationL -> IdL
    var = fst

    deps :: IdL -> [DeclarationL] -> [DeclarationL] -> [IdL]
    deps x prevs []                = error "Empty dependencies."
    deps x prevs (decl@(y, e) : ds)
      | x == y    = intersect nodes (varsL e)
      | otherwise = deps x (decl:prevs) ds

-- Kosaraju
stronglyConnectedComponents :: forall a. Ord a => Graph a -> [[a]]
stronglyConnectedComponents graph =
    cc (transpose graph)
       (concat (cc graph (Map.keys graph)))
  where
    cc :: Ord a => Graph a -> [a] -> [[a]]
    cc _     []            = []
    cc graph stack@(v : _) =
      let component = dfs graph v in
        cc (foldr removeNode graph component)
           (foldr removeS stack component) ++
        [component]

    removeS :: Eq a => a -> [a] -> [a]
    removeS v stack = stack \\ [v]

type ComponentIdx = Integer

data Dependency a = DpFunctions [a] | DpAcyclic a
  deriving (Show, Eq)

dependencySortL :: [DeclarationL] -> [Dependency DeclarationL]
dependencySortL decls =
    reverse $ map idsToDecl sortedIds
  where
    invflat :: (a, [b]) -> [(b, a)]
    invflat (x, ys) = zip ys (repeat x)

    nodeDepgraph :: DepGraph
    nodeDepgraph = dependencyGraphL decls

    sccs :: [(ComponentIdx, [IdL])]
    sccs = zip [0..] (stronglyConnectedComponents nodeDepgraph)

    idxToNodes :: Map.Map ComponentIdx [IdL]
    idxToNodes = Map.fromList sccs

    nodeToIdx :: Map.Map IdL ComponentIdx
    nodeToIdx = Map.fromList (concatMap invflat sccs)

    componentDeps :: ComponentIdx -> [ComponentIdx]
    componentDeps idx = nub $ do
      v <- Map.findWithDefault [] idx idxToNodes
      w <- Map.findWithDefault [] v nodeDepgraph
      return $ Map.findWithDefault (error "") w nodeToIdx

    componentDepgraph :: Graph ComponentIdx
    componentDepgraph =
        Map.fromList $ map
          (\ (idx, _) -> (idx, componentDeps idx \\ [idx]))
          sccs

    idToDecl :: IdL -> DeclarationL
    idToDecl id = Map.findWithDefault (error "") id m
      where m = Map.fromList $
                  map (\ decl@(x, _) -> (x, decl)) decls

    idsToDecl :: [IdL] -> Dependency DeclarationL
    idsToDecl [id]
      | id `elem` Map.findWithDefault [] id nodeDepgraph =
        DpFunctions [idToDecl id]
      | otherwise =
        let decl = idToDecl id in
          case decl of
            (_, (C.LamL _ _ _)) -> DpFunctions [decl]
            (_, (C.Lam _ _)) -> DpFunctions [decl]
            _                    -> DpAcyclic decl
    idsToDecl lst  = DpFunctions $ map idToDecl lst

    sortedIds :: [[IdL]]
    sortedIds =
      map (\ idx -> Map.findWithDefault [] idx idxToNodes)
          (topoSort componentDepgraph)

