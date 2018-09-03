import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Lib
import Data.Either

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