module Point (
  Point(..),
  emptyPoint,
  updateRow,
  updateIndex,
  updateColumn,
) where

data Point = Point { row :: Integer, column :: Integer, index :: Integer } deriving (Eq, Show) -- index is the number of the current character respect the complete file

emptyPoint :: Point
emptyPoint = Point 0 0 0

updateRow :: Point -> Point
updateRow Point { row = r, column = c, index = i } = Point (r + 1) 0 i

updateIndex :: Integer -> Point -> Point
updateIndex n Point { row = r, column = c, index = i } = Point r c (i + n)

updateColumn :: Integer -> Point -> Point
updateColumn n Point { row = r, column = c, index = i } = Point r (c + n) i