module PrimOps 
  ( primOps
  , plus
  , minus
  , mul
  , sizedOps
  ) where

import qualified Data.Map.Strict as Map

import Types
  ( Expression(..)
  , Type(..)
  , IdentifierName
  )

import PrimTypes
  ( integer
  , scalar
  , vector
  , function
  )

primOps :: [Expression]
primOps = [ plus, minus, mul ]

plus :: Expression
plus = NativeExpression "plus" [integer] integer

minus :: Expression
minus = NativeExpression "minus" [integer] integer

mul :: Expression
mul = NativeExpression "mul" [integer] integer

sizedOps :: IdentifierName -> Integer -> Type -> Expression
sizedOps "join" n ofType = NativeExpression "join" [ofType, (vector n ofType)] (vector (n + 1) ofType)