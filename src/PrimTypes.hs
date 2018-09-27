module PrimTypes
  ( integer
  , scalar
  , vector
  , function
  ) where

import Types
  ( Type(..)
  , Primitive(..)
  )


integer :: Type
integer = Type "Integer"

scalar :: Type
scalar = Type "Scalar"

vector :: Integer -> Type -> Type
vector n t = NestedType "Vector" [DataType (PrimInt n), t]

function :: [Type] -> Type
function args = NestedType "Function" args