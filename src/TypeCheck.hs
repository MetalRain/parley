module TypeCheck
    ( identType
    , primType
    , exprType
    ) where

import Types
  ( LineGroup(..)
  , Assignment(..)
  , Identifier(..)
  , Type(..)
  , TFunction(..)
  , TVector(..)
  , Primitive(..)
  , Expression(..)
  )

identType :: Identifier -> Type
identType (Identifier _ t) = t

primType :: Primitive -> Type
primType (PrimInt _)    = Type "Integer"
primType (PrimScalar _) = Type "Scalar"
primType (PrimVector (TVector size [])) = SizedType "Vector" 0 AnyType
primType (PrimVector (TVector size ps)) = SizedType "Vector" size ((map primType ps) !! 0)
primType (PrimFunc (TFunction args e))  = NestedType "Function" ((map identType args) ++ [exprType e])

exprType :: Expression -> Type
exprType _ = AnyType