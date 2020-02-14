
module AST() where

data Program = Program {
                 programDeclarations :: [Declaration]
               }

data Declaration = 
  TypeDeclaration

