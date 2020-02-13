module Position (
  Position,
  positionFilename, positionSource,
  positionLine, positionColumn, positionIndex,
  unknownPosition,
  initialPosition,
  updatePosition
) where

data Position = Position {
                  positionFilename :: String,
                  positionSource   :: String,
                  positionLine     :: Integer,
                  positionColumn   :: Integer,
                  positionIndex    :: Integer -- index inside the source
                }
  deriving (Eq, Show)

unknownPosition :: Position
unknownPosition = Position {
                    positionFilename = "?",
                    positionSource   = "?",
                    positionLine     = 0,
                    positionColumn   = 0,
                    positionIndex    = 0
                  }

initialPosition :: FilePath -> String -> Position
initialPosition filename source = Position {
                                    positionFilename = filename,
                                    positionSource   = source,
                                    positionLine     = 1,
                                    positionColumn   = 1,
                                    positionIndex    = 0
                                  }

updatePosition :: String -> Position -> Position
updatePosition []          pos = pos
updatePosition ('\n' : cs) pos =
  updatePosition cs (pos { positionLine   = positionLine pos + 1,
                           positionColumn = 1,
                           positionIndex  = positionIndex pos + 1 })
updatePosition (_ : cs)   pos =
  updatePosition cs (pos { positionColumn = positionColumn pos + 1,
                           positionIndex  = positionIndex pos + 1 })

