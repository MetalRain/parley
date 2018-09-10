import Test.Hspec
import Test.QuickCheck
import qualified ParserSpec
import qualified TypeCheckSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Parser" ParserSpec.spec
  describe "TypeCheck" TypeCheckSpec.spec