module Types
    ( TypeName
    , IdentifierName
    , FunctionName
    , Type(..)
    , Identifier(..)
    , TInteger
    , TScalar
    , TVector(..)
    , TFunction(..)
    , Primitive(..)
    , Expression(..)
    , Argument(..)
    , Assignment(..)
    , Indentation
    , Line(..)
    , LineGroup(..)
    , mkScalar
    ) where

import Data.Ratio ( (%) )

type TypeName = String
type IdentifierName = String
type FunctionName = IdentifierName

data Type = AnyType
          | Type TypeName
          | SizedType TypeName Integer Type
          | NestedType TypeName [Type] deriving (Eq, Show)

-- Primary types
type TInteger = Integer
type TScalar = Rational
mkScalar :: (Integral a) => a -> a -> TScalar
mkScalar a b  = (fromIntegral a) % (fromIntegral b)

data TVector = TVector TInteger [Primitive] deriving (Eq, Show)

data Identifier = Identifier IdentifierName Type deriving (Eq, Show)
type Source = Identifier
{-
Function definition:
IdentifierName = a: T b: T -> Expression
  Assignments 
-}
data TFunction = TFunction [Source] Expression deriving (Eq, Show)


data Primitive = PrimInt TInteger
               | PrimScalar TScalar
               | PrimVector TVector
               | PrimFunc TFunction
                deriving (Eq, Show)
data Argument = ArgIdent IdentifierName
              | ArgPrim Primitive
               deriving (Eq, Show)


{- 
Expressions:
1 plus 2
f 1 2
f (1,2,3) a
-}
data Expression = Expression FunctionName [Argument] deriving (Eq, Show)


{-
Assignment:
a = 1
b = 2.1
c = (1, 2, 4)
d = a b -> a + b

Evaluation:
a <- Expression
-}
data Assignment = PrimAssign IdentifierName Primitive
                | ExprAssign IdentifierName Expression
                 deriving (Eq, Show)


type Indentation = Integer
data Line = Line Indentation Assignment deriving (Eq, Show)
data LineGroup = LineGroup Indentation Assignment [LineGroup] deriving (Eq, Show)