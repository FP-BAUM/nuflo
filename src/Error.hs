module Error (Error(..), ErrorType(..), errorAtUnknown) where

import Position(Position, unknownPosition)

data Error = Error {
               errorType     :: ErrorType,
               errorPosition :: Position,
               errorMessage  :: String
             }
  deriving (Eq, Show)

data ErrorType =
    LexerErrorInvalidCharacter
  | LexerErrorMalformedName
  | LexerErrorLayout
  | ReaderErrorCyclicDependencies
  deriving (Eq, Show)

errorAtUnknown :: ErrorType -> String -> Error
errorAtUnknown typ msg = Error typ unknownPosition msg

