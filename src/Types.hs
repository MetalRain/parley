{-# LANGUAGE TypeSynonymInstances #-}

module Types
    ( TypeName
    , IdentifierName
    , FunctionName
    , Type(..)
    , Context(..)
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
    , AST(..)
    , mkScalar
    , mkContext
    ) where

import Data.List ( intersperse, intercalate )
import Data.Ratio ( (%) )
import qualified Data.Map.Strict as Map

withSpaces :: [String] -> String
withSpaces = intercalate " "

type TypeName = String
type IdentifierName = String
type FunctionName = IdentifierName
type PrimOpName = IdentifierName

data Type = UnresolvedType
          | Type TypeName
          | DataType Primitive
          | NestedType TypeName [Type]
          | VariableType IdentifierName deriving (Eq)
instance Show Type where
  show (UnresolvedType) = "???"
  show (Type name) = name
  show (DataType prim) = show prim
  show (NestedType name types) = name ++ "(" ++ (intercalate ", " $ (map show types)) ++ ")"
  show (VariableType name) = name

data Context = Context (Map.Map IdentifierName Type) deriving (Eq)
mkContext :: [(IdentifierName, Type)] -> Context
mkContext xs = Context $ Map.fromList xs

instance Show Context where
  show (Context m) = intercalate "\n" (map (\(name, t) -> withSpaces [name, "=", show t]) $ Map.toList m)

-- Primary types
type TInteger = Integer
type TScalar = Rational
mkScalar :: (Integral a) => a -> a -> TScalar
mkScalar a b  = (fromIntegral a) % (fromIntegral b)

data TVector = TVector TInteger [Primitive] deriving (Eq)
instance Show TVector where
  show (TVector i prims) = "(" ++ (intercalate ", " $ map show prims) ++ ")"

data Identifier = Identifier IdentifierName Type deriving (Eq)
instance Show Identifier where
  show (Identifier name t) = name ++ ": " ++ show t

type Source = Identifier
{-
Function definition:
IdentifierName = a: T b: T -> Expression
  Assignments 
-}
data TFunction = TFunction [Source] Expression deriving (Eq)
instance Show TFunction where
  show (TFunction args expr) = withSpaces (map show args) ++ " -> " ++ (show expr)


data Primitive = PrimInt TInteger
               | PrimScalar TScalar
               | PrimVector TVector
               | PrimFunc TFunction
                deriving (Eq)
instance Show Primitive where
  show (PrimInt i) = show i
  show (PrimScalar s) = show s
  show (PrimVector (TVector i p)) = "Vector(" ++ (show i) ++ ", " ++ (show p) ++ ")"
  show (PrimFunc (TFunction args expr)) = withSpaces (map show args) ++ " -> " ++ show expr

data Argument = ArgIdent IdentifierName
              | ArgPrim Primitive
               deriving (Eq)
instance Show Argument where
  show (ArgIdent name) = name
  show (ArgPrim prim) = show prim


{- 
Expressions:
1 plus 2
f 1 2
f (1,2,3) a
-}
data Expression = Expression FunctionName [Argument]
                | NativeExpression PrimOpName [Type] Type
                 deriving (Eq)
instance Show Expression where
  show (Expression name args) = withSpaces (name : (map show args))
  show (NativeExpression name inputTypes outType) = withSpaces $ [name] <> (map show inputTypes) <> [" -> ", show outType]


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
                | TypeAssign Type Type
                 deriving (Eq)
instance Show Assignment where
  show (PrimAssign name prim) = withSpaces [name, "=", show prim]
  show (ExprAssign name expr) = withSpaces [name, "<-", show expr]
  show (TypeAssign alias t) = withSpaces ["alias", show alias, "=", show t]

type Indentation = Integer
data Line = Line Indentation Assignment deriving (Eq, Show)
data LineGroup = LineGroup Indentation Assignment [LineGroup] deriving (Eq)
instance Show LineGroup where
  show (LineGroup i a lgs) = (replicate (fromIntegral i) ' ') ++ (show a) ++ "\n" ++ (intercalate "" (map show lgs))
data AST = AST Assignment Context [AST] deriving (Eq, Show)
