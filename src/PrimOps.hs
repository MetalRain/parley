module PrimOps 
  ( primOps
  , plus
  , minus
  , mul
  , join
  , idx
  , get
  ) where

import qualified Prelude as P
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
  , laxVector
  , function
  )

primOps :: [Expression]
primOps = [ id
          , plus, minus, mul
          , splus, sminus, smul, sdiv
          , join, idx, get
          ]

-- Common ops
id :: Expression
id = NativeExpression "id" [VariableType "t"] (VariableType "t")

-- Integer ops
plus :: Expression
plus = NativeExpression "plus" [integer, integer] integer

minus :: Expression
minus = NativeExpression "minus" [integer, integer] integer

mul :: Expression
mul = NativeExpression "mul" [integer, integer] integer

-- Scalar ops
splus :: Expression
splus = NativeExpression "splus" [scalar, scalar] scalar

sminus :: Expression
sminus = NativeExpression "sminus" [scalar, scalar] scalar

smul :: Expression
smul = NativeExpression "smul" [scalar, scalar] scalar

sdiv :: Expression
sdiv = NativeExpression "sdiv" [scalar, scalar] scalar

-- Vector ops
join :: Expression
join = NativeExpression "join" [laxVector (VariableType "n") (VariableType "t"), VariableType "t"] (laxVector (VariableType "n2") (VariableType "t"))

idx :: Expression
idx = NativeExpression "idx" [laxVector (VariableType "n") (VariableType "t"), VariableType "t"] integer

get :: Expression
get = NativeExpression "get" [laxVector (VariableType "n") (VariableType "t"), integer] (VariableType "t")