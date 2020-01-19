module TContext (
  TContext(..),
  context,
  applyIndexContext,
  incrementRow,
  incrementColumn,
  incrementIndex,
  pointFromContext,
) where

import Point

data TContext = TContext { fileName :: String , source :: String , tIndex :: Point } deriving (Eq, Show)

context :: String -> String -> TContext
context name source = TContext name source emptyPoint

applyIndexContext :: (Point -> Point) -> TContext -> TContext
applyIndexContext f (TContext {fileName = name, source = s, tIndex = point}) = TContext name s (f point)

incrementRow :: TContext -> TContext
incrementRow = applyIndexContext updateRow

incrementColumn :: Integer -> TContext -> TContext
incrementColumn n = applyIndexContext (updateColumn n)

incrementIndex :: Integer -> TContext -> TContext
incrementIndex n = applyIndexContext (updateIndex n)

pointFromContext :: TContext -> Point
pointFromContext TContext {fileName = name, source = s, tIndex = point} = point