module CompilerSpec ( spec ) where

import Test.Hspec
import Test.QuickCheck hiding (function, vector)
import Compiler

testCompile :: FilePath -> IO CompileResult
testCompile f = do
  code <- readFile f
  let result = compile code
  return result

spec :: Spec
spec = do
  describe "compile" $ do
    it "can compile fib" $ do
      result <- testCompile "examples/fib.par"
      wasSuccessful result `shouldBe` True

    it "can compile vec-len" $ do
      result <- testCompile "examples/vec-len.par"
      wasSuccessful result `shouldBe` True

    it "can compile plus" $ do
      result <- testCompile "examples/plus.par"
      wasSuccessful result `shouldBe` True

    it "can compile prime" $ do
      result <- testCompile "examples/prime.par"
      wasSuccessful result `shouldBe` True

    it "can compile out-order" $ do
      result <- testCompile "examples/out-order.par"
      wasSuccessful result `shouldBe` True