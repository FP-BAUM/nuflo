module Error (Error(..), ErrorType(..), ErrorMessage, errorAtUnknown) where

import Position(Position, unknownPosition)

type ErrorMessage = String

data Error = Error {
               errorType     :: ErrorType,
               errorPosition :: Position,
               errorMessage  :: ErrorMessage
             }
  deriving (Eq, Show)

data ErrorType =
    InternalError
  | LexerErrorInvalidCharacter
  | LexerErrorMalformedName
  | LexerErrorLayout
  | LexerErrorUnclosedComment
  | ReaderErrorCyclicDependencies
  | ReaderErrorModuleNameMismatch
  | ReaderErrorMissingFile
  | ParseError
  | ModuleSystemError
  | KindErrorMalformedDatatype
  | KindErrorTypeAlreadyDeclared
  | KindErrorTypeUndeclared
  | KindErrorClassAlreadyDeclared
  | KindErrorClassUndeclared
  | KindErrorConstructorShouldReturnDataType
  | KindErrorMalformedType
  | KindErrorUnifyOccursCheck
  | KindErrorUnifyClash
  | KindErrorInstanceMustBeDatatype
  | TypeErrorVariableAlreadyDeclared
  deriving (Eq, Show)

errorAtUnknown :: ErrorType -> String -> Error
errorAtUnknown typ msg = Error typ unknownPosition msg

