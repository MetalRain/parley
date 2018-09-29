module ParserSpec ( spec ) where

import Test.Hspec
import Test.QuickCheck
import Parser
import Types
import Data.Either (Either(..), isLeft)

spec :: Spec
spec = do
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

  describe "typeParser" $ do
    it "accepts simple type" $ do
      testParser typeParser "Integer" `shouldBe` (Right (Type "Integer"))

    it "accepts integer data type" $ do
      testParser typeParser "1" `shouldBe` (Right (DataType (PrimInt 1)))

    it "accepts scalar data type" $ do
      testParser typeParser "1/3" `shouldBe` (Right (DataType (PrimScalar (mkScalar 1 3))))

    it "accepts vector data type" $ do
      testParser typeParser "(1, 2, 3)" `shouldBe` (Right (DataType (PrimVector (TVector 3 [PrimInt 1, PrimInt 2, PrimInt 3]))))

    it "accepts function data type" $ do
      testParser typeParser "a: Integer -> f a" `shouldBe` (Right (DataType (PrimFunc (TFunction [Identifier "a" (Type "Integer")] (Expression "f" [(ArgIdent "a")])))))

    it "accepts function nested types" $ do
      testParser typeParser "Function(Integer, Integer, Integer)" `shouldBe` (Right (NestedType "Function" [Type "Integer", Type "Integer", Type "Integer"]))

    it "accepts vector nested types" $ do
      testParser typeParser "Vector(1, Integer)" `shouldBe` (Right (NestedType "Vector" [DataType (PrimInt 1), Type "Integer"]))

    it "accepts nested nested types" $ do
      testParser typeParser "T(S(A))" `shouldBe` (Right (NestedType "T" [NestedType "S" [Type "A"]]))

    it "accepts type variables" $ do
      testParser typeParser "a" `shouldBe` (Right (VariableType "a"))

    it "accepts type variables" $ do
      testParser typeParser "Vector(n, t)" `shouldBe` (Right (NestedType "Vector" [VariableType "n", VariableType "t"]))

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

  describe "expressionParser" $ do
    it "accepts prefix expression" $ do
      testParser expressionParser "f 1 2" `shouldBe` (Right (Expression "f" 
        [ ArgPrim (PrimInt 1)
        , ArgPrim (PrimInt 2) 
        ]))

    it "accepts infix expression" $ do
      testParser expressionParser "1 plus 2" `shouldBe` (Right (Expression "plus" 
        [ ArgPrim (PrimInt 1)
        , ArgPrim (PrimInt 2)
        ]))

    it "accepts identifiers as arguments" $ do
      testParser expressionParser "f a b c" `shouldBe` (Right (Expression "f" 
        [ ArgIdent "a"
        , ArgIdent "b"
        , ArgIdent "c"
        ]))

    it "accepts mixed argument prefix expressions" $ do
      testParser expressionParser "f a 1 c" `shouldBe` (Right (Expression "f" 
        [ ArgIdent "a"
        , ArgPrim (PrimInt 1)
        , ArgIdent "c"
        ]))

    it "rejects mixed argument infix expressions" $ do
      isLeft $ testParser expressionParser "1 div a"

    it "rejects invalid identifiers" $ do
      isLeft $ testParser expressionParser "1 + 1"

  describe "functionParser" $ do
    it "accepts simple function" $ do
      testParser functionParser "a: T b: S -> plus a b" `shouldBe` (Right (TFunction
        [ (Identifier "a" (Type "T"))
        , (Identifier "b" (Type "S"))
        ] 
        (Expression "plus" [ ArgIdent "a", ArgIdent "b" ])))

    it "can be used in vectorParser" $ do
      testParser vectorParser "(a: T -> add a, b: S -> sub b)" `shouldBe` (Right (TVector 2
        [ (PrimFunc (TFunction [ (Identifier "a" (Type "T")) ] (Expression "add" [ ArgIdent "a"])))
        , (PrimFunc (TFunction [ (Identifier "b" (Type "S")) ] (Expression "sub" [ ArgIdent "b"])))
        ]))

    it "rejects untyped functions" $ do
      isLeft $ testParser functionParser "a -> f a"

    it "rejects partially typed functions" $ do
      isLeft $ testParser functionParser "a: S b -> f b"

    it "rejects functions without return" $ do
      isLeft $ testParser functionParser "a: S b -> "

    it "rejects functions without arguments" $ do
      isLeft $ testParser functionParser "-> a"

    xit "accepts primitives as return values" $ do
      False
      -- Grammar needs some work
      --testParser functionParser "a: T b: S -> (1, 2)" `shouldBe` (Right (TFunction
      --  [ (Identifier "a" (Type "T"))
      --  , (Identifier "b" (Type "S"))
      --  ]
      --  (TVector 2 [ PrimInt 1, PrimInt 2 ])))

  describe "assignmentParser" $ do
    it "accepts integer assignment" $ do
      testParser assignmentParser "a = 1" `shouldBe` (Right (PrimAssign "a"
        (PrimInt 1)))

    it "accepts scalar assignment" $ do
      testParser assignmentParser "a = 1/2" `shouldBe` (Right (PrimAssign "a"
        (PrimScalar $ mkScalar 1 2)))

    it "accepts vector assignment" $ do
      testParser assignmentParser "a = (1,2)" `shouldBe` (Right (PrimAssign "a"
        (PrimVector (TVector 2 [PrimInt 1, PrimInt 2]))))

    it "accepts function assignment" $ do
      testParser assignmentParser "a = a: X -> f a" `shouldBe` (Right (PrimAssign "a"
        (PrimFunc (TFunction
          [ Identifier "a" (Type "X") ]
          ( Expression "f" [ ArgIdent "a" ] )))))

    it "accepts type aliases" $ do
      testParser assignmentParser "alias Vec3(t) = Vector(3, t)" `shouldBe` (Right (TypeAssign
        (NestedType "Vec3" [VariableType "t"])
        (NestedType "Vector" [DataType (PrimInt 3), VariableType "t"])))

  describe "linesParser" $ do
    it "accepts single line" $ do
      testParser linesParser "a = 1" `shouldBe` (Right
        [ Line 0 (PrimAssign "a" (PrimInt 1)) ]) 

    it "accepts program line by line" $ do
      let program = "v = (1, 2, 3)\n\
                    \mul = a: Integer b: Integer -> mul a1 b1\n\
                    \  a1 <- plus a a\n\
                    \  b1 <- minus b 1\n\
                    \square = a: Integer -> mul a a\n\
                    \res <- map square v"
      testParser linesParser program `shouldBe` (Right 
            [ Line 0 (PrimAssign "v" (PrimVector (TVector 3 [ PrimInt 1, PrimInt 2, PrimInt 3 ])))
            , Line 0 (PrimAssign "mul" (PrimFunc (TFunction
              [ (Identifier "a" (Type "Integer")), (Identifier "b" (Type "Integer")) ]
              (Expression "mul" [ ArgIdent "a1" , ArgIdent "b1" ]))))
            , Line 2 (ExprAssign "a1" (Expression "plus" [ ArgIdent "a", ArgIdent "a" ])) 
            , Line 2 (ExprAssign "b1" (Expression "minus" [ ArgIdent "b", ArgPrim (PrimInt 1) ]))
            , Line 0 (PrimAssign "square" (PrimFunc (TFunction
              [ (Identifier "a" (Type "Integer")) ]
              (Expression "mul" [ ArgIdent "a", ArgIdent "a" ]))))
            , Line 0 (ExprAssign "res" (Expression "map" [ ArgIdent "square", ArgIdent "v" ]))
            ])

    it "accepts comment lines" $ do
      let program = "# values\n\
                    \v = (1, 2, 3)"
      testParser linesParser program `shouldBe` (Right 
            [ CommentLine 0 " values"
            , Line 0 (PrimAssign "v" (PrimVector (TVector 3 [ PrimInt 1, PrimInt 2, PrimInt 3 ])))
            ])

    it "accepts empty lines" $ do
      let program = "  \n\
                    \v = (1, 2, 3)"
      testParser linesParser program `shouldBe` (Right 
            [ CommentLine 0 ""
            , Line 0 (PrimAssign "v" (PrimVector (TVector 3 [ PrimInt 1, PrimInt 2, PrimInt 3 ])))
            ])


  describe "lineGroupParser" $ do
    it "builds linegroup tree" $ do
      let program = "main = a: T -> id d\n\
                    \  d <- id b\n\
                    \  b <- id c\n\
                    \    c = 1"
      testParser lineGroupParser program `shouldBe` (Right
        [(LineGroup 0
          (PrimAssign "main" (PrimFunc (TFunction
            [ (Identifier "a" (Type "T")) ]
            (Expression "id" [ ArgIdent "d" ]))))
          [ (LineGroup 2
              (ExprAssign "d" (Expression "id" [ ArgIdent "b" ]))
              [])
          , (LineGroup 2
              (ExprAssign "b" (Expression "id" [ ArgIdent "c" ]))
            [ (LineGroup 4
                (PrimAssign "c" (PrimInt 1))
                []
              )
            ])
          ])])