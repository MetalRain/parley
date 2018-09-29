import Test.Hspec
import Test.QuickCheck
import qualified ParserSpec
import qualified ASTSpec
import qualified CompilerSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Parser" ParserSpec.spec
  describe "AST" ASTSpec.spec
  describe "Compiler" CompilerSpec.spec