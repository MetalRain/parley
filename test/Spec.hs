import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Lib
import Data.Either
import Data.Ratio ((%))

main :: IO ()
main = hspec $ do
  describe "identifierNameParser" $ do
    it "accepts single letter names" $ do
      testParser identifierNameParser "a" `shouldBe` (Right "a")

    it "accepts names starting with lowercase" $ do
      testParser identifierNameParser "aVar1" `shouldBe` (Right "aVar1")

    it "accepts names with only lowercase letters" $ do
      testParser identifierNameParser "variable" `shouldBe` (Right "variable")

    it "rejects empty names" $ do
      isLeft $ testParser identifierNameParser ""

    it "rejects names with spaces" $ do
      isLeft $ testParser identifierNameParser " a"

    it "rejects names starting with number" $ do
      isLeft $ testParser identifierNameParser "1a"

    it "rejects names starting with uppercase char" $ do
      isLeft $ testParser identifierNameParser "Type"

    it "rejects names starting with underscore" $ do
      isLeft $ testParser identifierNameParser "__supersecret"

  describe "typeNameParser" $ do
    it "accepts single letter names" $ do
      testParser typeNameParser "N" `shouldBe` (Right "N")

    it "accepts names starting with uppercase" $ do
      testParser typeNameParser "Tree" `shouldBe` (Right "Tree")

    it "accepts names with only uppercase letters" $ do
      testParser typeNameParser "AST" `shouldBe` (Right "AST")

    it "rejects empty names" $ do
      isLeft $ testParser typeNameParser ""

    it "rejects names with spaces" $ do
      isLeft $ testParser typeNameParser " a"

    it "rejects names starting with number" $ do
      isLeft $ testParser typeNameParser "1a"

    it "rejects names starting with lowercase char" $ do
      isLeft $ testParser typeNameParser "tyPe"

    it "rejects names starting with underscore" $ do
      isLeft $ testParser typeNameParser "__supersecret"

  describe "identifierParser" $ do
    it "accepts valid typed identifier" $ do
      testParser identifierParser "a: T" `shouldBe` (Right (Identifier "a" (Type "T")))

    it "accepts valid typed identifier with more spacing" $ do
      testParser identifierParser "a  :  T" `shouldBe` (Right (Identifier "a" (Type "T")))

    it "accepts valid typed identifier with changed spacing" $ do
      testParser identifierParser "a     : T" `shouldBe` (Right (Identifier "a" (Type "T")))

    it "rejects untyped identifier" $ do
      isLeft $ testParser identifierParser "a"

    it "rejects unindentifier type" $ do
      isLeft $ testParser identifierParser ": T"

  describe "integerParser" $ do
    it "accepts positive integer" $ do
      testParser integerParser "123" `shouldBe` (Right 123)

    it "accepts zero" $ do
      testParser integerParser "0" `shouldBe` (Right 0)

    it "accepts negative numbers" $ do
      testParser integerParser "-9999" `shouldBe` (Right (-9999))

    it "reject leading zeros" $ do
      isLeft $ testParser integerParser "0800"

    it "reject double signs" $ do
      isLeft $ testParser integerParser "--8"

    it "reject plus signs" $ do
      isLeft $ testParser integerParser "+8"

  describe "scalarParser" $ do
    it "accepts fraction" $ do
      testParser scalarParser "2 / 3" `shouldBe` (Right (mkScalar 2 3))

    it "accepts fraction without space" $ do
      testParser scalarParser "3/9" `shouldBe` (Right (mkScalar 3 9))

    it "accepts zero numerator" $ do
      testParser scalarParser "0 / 3" `shouldBe` (Right (mkScalar 0 3))

    it "accepts negative numerator" $ do
      testParser scalarParser "-1 / 5" `shouldBe` (Right (mkScalar (-1) 5))

    it "rejects zero denumerator" $ do
      isLeft $ testParser scalarParser "2 / 0"

    it "rejects negative denumerator" $ do
      isLeft $ testParser scalarParser "1 / -2"

    it "rejects missing numerator" $ do
      isLeft $ testParser scalarParser " / 1"

    it "rejects missing denumerator" $ do
      isLeft $ testParser scalarParser "1 / "

  describe "vectorParser" $ do
    it "accepts integer vector" $ do
      testParser vectorParser "(1, 2, 3)" `shouldBe` (Right (TVector 3 [PrimInt 1, PrimInt 2, PrimInt 3]))

    it "accepts vector without spaces" $ do
      testParser vectorParser "(1,2,3)" `shouldBe` (Right (TVector 3 [PrimInt 1, PrimInt 2, PrimInt 3]))

    it "accepts scalar vector" $ do
      testParser vectorParser "(1/5, 2/1, 1/3, 1/4)" `shouldBe` (Right (TVector 4 
        [ (PrimScalar (mkScalar 1 5))
        , (PrimScalar (mkScalar 2 1))
        , (PrimScalar (mkScalar 1 3))
        , (PrimScalar (mkScalar 1 4))
        ]))

    it "accepts nested integer vectors" $ do
      testParser vectorParser "((1, 2), (3, 4), (5, 6))" `shouldBe` (Right (TVector 3 
        [ PrimVector (TVector 2 [PrimInt 1, PrimInt 2])
        , PrimVector (TVector 2 [PrimInt 3, PrimInt 4])
        , PrimVector (TVector 2 [PrimInt 5, PrimInt 6])
        ]))

    it "rejects missing left parens" $ do
      isLeft $ testParser vectorParser "1,2)"

    it "rejects missing right parens" $ do
      isLeft $ testParser vectorParser "(1,2"

    it "rejects missing commas" $ do
      isLeft $ testParser vectorParser "(1 2)"

    xit "reject mixed size nested vectors" $ do
      isLeft $ testParser vectorParser "((1, 2), (3), ())"
