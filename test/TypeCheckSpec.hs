module TypeCheckSpec ( spec ) where

import Test.Hspec
import Test.QuickCheck
import TypeCheck
import Types
import Data.Either (Either(..), isLeft)

spec :: Spec
spec = do
  describe "primType" $ do
    it "returns simple type for integers" $ do
      primType (PrimInt 1) `shouldBe` (Type "Integer")

    it "returns simple type for scalars" $ do
      primType (PrimScalar (mkScalar 1 2)) `shouldBe` (Type "Scalar")

    it "returns sized type for vectors" $ do
      primType (PrimVector (TVector 2 [PrimInt 3, PrimInt 4])) `shouldBe` (SizedType "Vector" 2 (Type "Integer"))

    it "returns nested type for functions" $ do
      primType (PrimFunc (TFunction 
        [ Identifier "a" (Type "T")
        , Identifier "v" (SizedType "Vector" 1 (Type "T"))
        ]
        (Expression "contains"
          [ ArgIdent "v"
          , ArgIdent "a" 
          ]))) `shouldBe` (NestedType "Function" [Type "T", (SizedType "Vector" 1 (Type "T")), AnyType])