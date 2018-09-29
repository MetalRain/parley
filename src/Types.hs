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

import Prelude hiding (showList)
import Data.List ( foldl', intercalate )
import Data.Ratio ( (%) )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import PrettyShow
  ( showMany
  , withCommas
  , withRows
  , withSpaces
  , showKeyValue
  , showIdentType
  , indent
  , indentRows
  )

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
  show (NestedType name types) = name ++ "(" ++ (withCommas $ showMany types) ++ ")"
  show (VariableType name) = name

data Context = Context (Map.Map IdentifierName Type) deriving (Eq)
mkContext :: [(IdentifierName, Type)] -> Context
mkContext xs = Context $ Map.fromList xs

instance Show Context where
  show (Context m) = withRows (map (\(k,v) -> "# " ++ showIdentType (k,v)) $ Map.toList m)

-- Primary types
type TInteger = Integer
type TScalar = Rational
mkScalar :: (Integral a) => a -> a -> TScalar
mkScalar a b  = (fromIntegral a) % (fromIntegral b)

data TVector = TVector TInteger [Primitive] deriving (Eq)
instance Show TVector where
  show (TVector i prims) = "(" ++ (withCommas $ showMany prims) ++ ")"

data Identifier = Identifier IdentifierName Type deriving (Eq)
instance Show Identifier where
  show (Identifier name t) = showIdentType (name, t)

type Source = Identifier
{-
Function definition:
IdentifierName = a: T b: T -> Expression
  Assignments 
-}
data TFunction = TFunction [Source] Expression deriving (Eq)
instance Show TFunction where
  show (TFunction args expr) = withSpaces (showMany args) ++ " -> " ++ show expr


data Primitive = PrimInt TInteger
               | PrimScalar TScalar
               | PrimVector TVector
               | PrimFunc TFunction
                deriving (Eq)
instance Show Primitive where
  show (PrimInt i) = show i
  show (PrimScalar s) = show s
  show (PrimVector (TVector i p)) = "(" ++ (withCommas $ showMany p) ++ ")"
  show (PrimFunc (TFunction args expr)) = withSpaces (showMany args) ++ " -> " ++ show expr

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
  show (Expression name args) = withSpaces (name : (showMany args))
  show (NativeExpression name inputTypes outType) = withSpaces $ [name] <> (showMany inputTypes) <> [" -> ", show outType]


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
  show (PrimAssign name prim) = showKeyValue (name, prim)
  show (ExprAssign name expr) = withSpaces [name, "<-", show expr]
  show (TypeAssign alias t) = withSpaces ["alias", showKeyValue (show alias, t)]

type Indentation = Integer
data Line = Line Indentation Assignment deriving (Eq, Show)
data LineGroup = LineGroup Indentation Assignment [LineGroup] deriving (Eq)
instance Show LineGroup where
  show (LineGroup i a lgs) = withRows [(indent (fromIntegral i) (show a)), (intercalate "" (showMany lgs))]
data AST = AST Assignment Context [AST] deriving (Eq)


argumentIdentifierNames :: Argument -> [IdentifierName]
argumentIdentifierNames (ArgIdent n) = [n]
argumentIdentifierNames (ArgPrim _) = []

identifiersInExpression :: Expression -> [IdentifierName]
identifiersInExpression (Expression n args) = foldl' (++) [n] (map argumentIdentifierNames args)
identifiersInExpression (NativeExpression n _ _) = [n]

identifierName :: Identifier -> IdentifierName
identifierName (Identifier n _) = n

identifiersInFunction :: TFunction -> [IdentifierName]
identifiersInFunction (TFunction args expr) = (map identifierName args) ++ identifiersInExpression expr

identifiersInAssignment :: Assignment -> [IdentifierName]
identifiersInAssignment (PrimAssign n (PrimFunc f)) = [n] ++ identifiersInFunction f
identifiersInAssignment (PrimAssign n _) = [n]
identifiersInAssignment (ExprAssign n e) = (n : identifiersInExpression e)

reducedContext :: Context -> Assignment -> Context
reducedContext (Context m) a = Context $ Map.restrictKeys m $ Set.fromList $ identifiersInAssignment a

instance Show AST where
  show (AST a c as) = indentRows 2 $ withRows (show a : (indentRows 2 $ show $ reducedContext c a) : showMany as)
