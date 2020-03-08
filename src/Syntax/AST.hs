{-# LANGUAGE DuplicateRecordFields #-}

module Syntax.AST(
         AnnProgram(..), Program,
         AnnDeclaration(..), Declaration,
         AnnSignature(..), Signature,
         AnnEquation(..), Equation,
         AnnConstraint(..), Constraint,
         AnnExpr(..), Expr,
         eraseAnnotations, exprIsVariable, exprHeadVariable
       ) where

import Position(Position)
import Syntax.Name(QName)

data AnnProgram a = Program {
                      programDeclarations :: [AnnDeclaration a]
                    }
  deriving Eq

type Program = AnnProgram Position

-- Annotated declaration
data AnnDeclaration a = 
    DataDeclaration {
      annotation       :: a,
      dataTypeName     :: AnnExpr a,
      dataConstructors :: [AnnSignature a]
    }
  | TypeDeclaration {
      annotation :: a,
      typeName   :: AnnExpr a,
      typeValue  :: AnnExpr a
    }
  | TypeSignature {
      typeSignature :: AnnSignature a
    }
  | ValueDeclaration {
      declEquation :: AnnEquation a
    }
  | ClassDeclaration {
      annotation    :: a,
      className     :: QName,
      classTypeName :: QName,
      classMethods  :: [AnnSignature a]
    }
  | InstanceDeclaration {
      annotation          :: a,
      instanceClassName   :: QName,
      instanceType        :: AnnExpr a,
      instanceConstraints :: [AnnConstraint a],
      instanceMethods     :: [AnnEquation a]
    }
  deriving Eq

data AnnSignature a = Signature {
                        annotation           :: a,
                        signatureName        :: QName,
                        signatureType        :: AnnExpr a,
                        signatureConstraints :: [AnnConstraint a]
                      } deriving Eq

data AnnEquation a = Equation {
                       annotation :: a,
                       equationLHS  :: AnnExpr a,
                       equationRHS  :: AnnExpr a
                     } deriving Eq

data AnnConstraint a = Constraint {
                         annotation          :: a,
                         constraintClassName :: QName,
                         constraintTypeName  :: QName
                       } deriving Eq

-- Annotated expression
data AnnExpr a =
    EVar a QName                           -- variable
  | EInt a Integer                         -- integer constant
  | EApp a (AnnExpr a) (AnnExpr a)         -- application
  | ELambda a [AnnExpr a] (AnnExpr a)      -- lambda
  | EWhere a [AnnEquation a]               -- where
  | ELet a [AnnDeclaration a] (AnnExpr a)  -- let
  deriving Eq

type Declaration = AnnDeclaration Position
type Constraint  = AnnConstraint Position
type Signature   = AnnSignature Position
type Equation    = AnnEquation Position
type Expr        = AnnExpr Position

--

class EraseAnnotations f where
  eraseAnnotations :: f a -> f ()

instance EraseAnnotations AnnProgram where
  eraseAnnotations (Program x) = Program (map eraseAnnotations x)

instance EraseAnnotations AnnDeclaration where
  eraseAnnotations (DataDeclaration _ x y) =
    DataDeclaration () (eraseAnnotations x) (map eraseAnnotations y)
  eraseAnnotations (TypeDeclaration _ x y) =
    TypeDeclaration () (eraseAnnotations x) (eraseAnnotations y)
  eraseAnnotations (TypeSignature x) =
    TypeSignature (eraseAnnotations x)
  eraseAnnotations (ValueDeclaration x) =
    ValueDeclaration (eraseAnnotations x)
  eraseAnnotations (ClassDeclaration _ x y z) =
    ClassDeclaration () x y (map eraseAnnotations z)
  eraseAnnotations (InstanceDeclaration _ x y z w) =
    InstanceDeclaration () x (eraseAnnotations y)
                             (map eraseAnnotations z)
                             (map eraseAnnotations w)

instance EraseAnnotations AnnSignature where
  eraseAnnotations (Signature _ x y z) =
    Signature () x (eraseAnnotations y) (map eraseAnnotations z)

instance EraseAnnotations AnnEquation where
  eraseAnnotations (Equation _ x y) =
    Equation () (eraseAnnotations x) (eraseAnnotations y)

instance EraseAnnotations AnnConstraint where
  eraseAnnotations (Constraint _ x y) = Constraint () x y

instance EraseAnnotations AnnExpr where
  eraseAnnotations (EVar _ q)     = EVar () q
  eraseAnnotations (EInt _ n)     = EInt () n
  eraseAnnotations (EApp _ e1 e2) = EApp () (eraseAnnotations e1)
                                            (eraseAnnotations e2)
  eraseAnnotations (ELet _ ds e)  = ELet () (map eraseAnnotations ds)
                                            (eraseAnnotations e)

--

exprIsVariable :: AnnExpr a -> Bool
exprIsVariable (EVar _ _) = True
exprIsVariable _          = False

exprHeadVariable :: AnnExpr a -> Maybe QName
exprHeadVariable (EVar _ q)    = return q
exprHeadVariable (EApp _ e1 _) = exprHeadVariable e1
exprHeadVariable _             = Nothing

---- Show

joinS :: String -> [String] -> String
joinS _   []       = ""
joinS _   [l]      = l
joinS sep (l : ls) = l ++ sep ++ joinS sep ls

joinLines :: [String] -> String
joinLines = joinS "\n"

indent :: String -> String
indent s = "  " ++ s

instance Show (AnnProgram a) where
  show (Program decls) = joinS "\n\n" (map show decls)

instance Show (AnnDeclaration a) where
  show (DataDeclaration _ typ constructorDeclarations) =
    joinLines (
      ["data " ++ show typ ++ " where"] ++
      map (indent . show) constructorDeclarations
    )
  show (TypeDeclaration _ typ val) =
    "type " ++ show typ ++ " = " ++ show val
  show (TypeSignature sig) = show sig
  show (ValueDeclaration equation) = show equation
  show (ClassDeclaration _ name typeName signatures) =
    joinLines (
      ["class " ++ show name ++ " " ++ show typeName ++ " where"] ++
      map (indent . show) signatures
    )
  show (InstanceDeclaration _ name typ constraints equations) =
    joinLines (
      ["instance " ++ show name ++ " " ++ show typ ++
                      showOptionalConstraints constraints ++ " where"] ++
      map (indent . show) equations
    )

instance Show (AnnSignature a) where
  show (Signature _ name typ constraints) =
      show name ++ " : " ++ show typ ++ showOptionalConstraints constraints

instance Show (AnnEquation a) where
  show (Equation _ lhs rhs) = show lhs ++ " = " ++ show rhs

instance Show (AnnConstraint a) where
  show (Constraint _ className typeName) =
    show className ++ " " ++ show typeName

showOptionalConstraints :: [AnnConstraint a] -> String
showOptionalConstraints [] = ""
showOptionalConstraints cs = 
  indent ("{"
       ++ joinS "; " (map show cs)
       ++ "}")

instance Show (AnnExpr a) where
  show (EVar _ qname) = show qname
  show (EInt _ n)     = show n
  show (EApp _ f x)   = "(" ++ show f ++ " " ++ show x ++ ")"
  show (ELambda _ params body) =
    "\\ " ++ joinS " " (map show params) ++ "-> { " ++ show body ++ " }"
  show (EWhere _ equations) =
    "{ " ++  joinS "\n" (map show equations) ++ "}"
  show (ELet _ ds e)  =
    "(let {" ++ joinS "; " (map show ds) ++ "} in " ++ show e ++ ")"