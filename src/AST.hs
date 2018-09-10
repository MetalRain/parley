module AST
    ( AST(..)
    ) where

import Types
  ( LineGroup(..)
  , Assignment(..)
  , Identifier(..)
  )

data Scope = Scope [Identifier] deriving (Eq, Show)
data AST a = ASTNode a [AST a] deriving (Eq, Show)

toAST :: LineGroup -> AST Assignment
toAST (LineGroup _ a ls) = ASTNode a (map toAST ls)