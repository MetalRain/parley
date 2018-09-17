module ASTSpec ( spec ) where

import Test.Hspec
import Test.QuickCheck hiding (function, vector)
import PrimTypes
import PrimOps
import AST
import Types
import Data.Either (Either(..), isLeft)

spec :: Spec
spec = do
  describe "primType" $ do
    let ctx = mkContext []
    it "returns simple type for integers" $ do
      primType ctx (PrimInt 1) `shouldBe` Right integer

    it "returns simple type for scalars" $ do
      primType ctx (PrimScalar (mkScalar 1 2)) `shouldBe` Right scalar

    it "returns sized type for vectors" $ do
      primType ctx (PrimVector (TVector 2 [PrimInt 3, PrimInt 4])) `shouldBe` Right (vector 2 integer)

    it "returns sized type for empty vectors" $ do
      primType ctx (PrimVector (TVector 0 [])) `shouldBe` Right (vector 0 (AnyType))

    it "returns & evaluates nested types for functions" $ do
      let t = Type "T"
      let vt = (vector 1 t)
      -- Integer simulates boolean
      let ctx = mkContext [ ("contains", (function [vector 1 t, t, integer]))
                          , ("a", integer)
                          , ("v", vector 2 integer)
                          ]
      primType ctx (PrimFunc (TFunction 
        [ Identifier "a" t
        , Identifier "v" vt
        ]
        (Expression "contains" [ ArgIdent "v", ArgIdent "a" ]))) `shouldBe` (Right (function [t, vt, integer]))

  describe "resolveTypes" $ do
    it "detects missing expression identifiers" $ do
      -- Include primOps in context
      let ctx = inheritContext (mkPrimOpsContext primOps) $ mkContext [("b", integer)]
      let e = (Expression "plus" [ ArgIdent "b" , ArgIdent "c" ])
      let a = (ExprAssign "a" e)
      let lg = (LineGroup 0 a [])
      let expected = enhanceErrorTrace (notDefined ctx e "c") a
      resolveTypes ctx lg `shouldBe` (Left expected)

    it "detects missing primOps" $ do
      let ctx = mkContext [("a", integer)]
      let e = (Expression "plus" [ ArgIdent "a" , ArgIdent "a" ])
      let a = (ExprAssign "a" e)
      let lg = (LineGroup 0 a [])
      let expected = enhanceErrorTrace (notDefined ctx e "plus") a
      resolveTypes ctx lg `shouldBe` (Left expected)