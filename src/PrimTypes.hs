module PrimTypes
  ( integer
  , scalar
  , vector
  , function
  ) where

import Types
  ( Type(..)
  )


integer :: Type
integer = Type "Integer"

scalar :: Type
scalar = Type "Scalar"

vector :: Integer -> Type -> Type
vector n t = SizedType "Vector" n t

function :: [Type] -> Type
function args = NestedType "Function" args