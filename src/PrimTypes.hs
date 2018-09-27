module PrimTypes
  ( integer
  , scalar
  , vector
  , laxVector
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
vector n t = laxVector (DataType (PrimInt n)) t

laxVector :: Type -> Type -> Type
laxVector nT t = NestedType "Vector" [nT, t]

function :: [Type] -> Type
function args = NestedType "Function" args