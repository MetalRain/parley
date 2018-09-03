{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( identifierNameParser
    , typeNameParser
    , identifierParser
    , integerParser
    , scalarParser
    , vectorParser
    , testParser
    , Type(..)
    , Identifier(..)
    , TInteger
    , TScalar
    , mkScalar
    , TVector(..)
    , Primitive(..)
    ) where

import Data.Char (isSpace)
import Data.Ratio ((%))
import Text.Parsec.Char ( alphaNum, char, digit, lower, oneOf, spaces, string, upper )
import Text.Parsec.Prim ( (<|>), Parsec(..), many, parse, try )
import Text.Parsec.Combinator ( choice, notFollowedBy, many1, option, sepBy, sepBy1 )

type ParserOf st a = Parsec String st a

type TypeName = String
type IdentifierName = String
type FunctionName = IdentifierName

data Type = Type TypeName deriving (Eq, Show)
type InputType = Type
type OutputType = Type

-- Primary types
type TInteger = Integer
type TScalar = Rational
data TVector = TVector TInteger [Primitive] deriving (Eq, Show)

data TFunction = TFunction [InputType] OutputType deriving (Eq, Show)

-- Syntax
data Identifier = Identifier IdentifierName Type deriving (Eq, Show)
data Primitive = PrimInt TInteger
               | PrimScalar TScalar
               | PrimVector TVector
                deriving (Eq, Show)
data Argument = ArgIdent IdentifierName
              | ArgPrim Primitive
               deriving (Eq, Show)


{- 
Expressions:
1 + 2
f 1 2
f (1,2,3) a
-}
data Expression = Expression FunctionName [Argument] deriving (Eq, Show)


type Source = Identifier              
type Sink = Identifier
{-
Function definition:
IdentifierName = a: T b: T -> c: T
-}
data FunctionDef = FunctionDef [Source] Sink deriving (Eq, Show)

{-
Assignment:
a = 1
b = 2.1
c = (1, 2, 4)
-}
data Assignment = Assignment IdentifierName Primitive deriving (Eq, Show)

{-
Applications:
a <- Expression
-}
data Application = Application IdentifierName Expression deriving (Eq, Show)

-- Utils

testParser p = parse p ""

surroundedWithSpaces :: String -> ParserOf st String
surroundedWithSpaces s = do
  _ <- spaces
  r <- string s
  _ <- spaces
  return r

mkScalar :: (Integral a) => a -> a -> TScalar
mkScalar a b  = (fromIntegral a) % (fromIntegral b)

-- Parsers

identifierNameParser :: ParserOf st IdentifierName
identifierNameParser = do
  start <- lower
  rest  <- many (alphaNum)
  return $ (start:rest)

typeNameParser :: ParserOf st TypeName
typeNameParser = do
  start <- upper
  rest  <- many (alphaNum)
  return $ (start:rest)

identifierParser :: ParserOf st Identifier
identifierParser = do
  idName   <- identifierNameParser
  _        <- surroundedWithSpaces ":"
  typeName <- typeNameParser
  return $ Identifier idName (Type typeName)

identifiersParser :: ParserOf st [Identifier]
identifiersParser = identifierParser `sepBy` (spaces)

negIntegerParser :: ParserOf st TInteger
negIntegerParser = do
  sign <- char '-'
  number <- oneOf "123456789"
  moreNumbers <- many (digit)
  notFollowedBy digit
  return $ read $ [sign, number] ++ moreNumbers

zeroIntegerParser :: ParserOf st TInteger
zeroIntegerParser = do
  zero <- char '0'
  notFollowedBy digit
  return 0

posIntegerParser :: ParserOf st TInteger
posIntegerParser = do
  number <- oneOf "123456789"
  moreNumbers <- many (digit)
  notFollowedBy digit
  return $ read $ [number] ++ moreNumbers

integerParser :: ParserOf st TInteger
integerParser = negIntegerParser <|> zeroIntegerParser <|> posIntegerParser

integerPrimParser :: ParserOf st Primitive
integerPrimParser = PrimInt <$> integerParser

scalarParser :: ParserOf st TScalar
scalarParser = do
  numerator   <- integerParser
  _           <- surroundedWithSpaces "/"
  denominator <- posIntegerParser
  return $ (fromIntegral numerator) % (fromIntegral denominator)

scalarPrimParser :: ParserOf st Primitive
scalarPrimParser = PrimScalar <$> scalarParser

primitiveParser :: ParserOf st Primitive
primitiveParser = (try vectorPrimParser)
                <|> (try scalarPrimParser)
                <|> (try integerPrimParser) 

vectorParser :: ParserOf st TVector
vectorParser = do
  start <- char '('
  prims <- primitiveParser `sepBy` (surroundedWithSpaces ",")
  end   <- char ')'
  return $ TVector (toInteger (length prims)) prims

vectorPrimParser :: ParserOf st Primitive
vectorPrimParser = PrimVector <$> vectorParser


argumentIdentifierParser :: ParserOf st Argument
argumentIdentifierParser = ArgIdent <$> identifierNameParser

argumentPrimitiveParser :: ParserOf st Argument
argumentPrimitiveParser = ArgPrim <$> primitiveParser


argumentParser :: ParserOf st Argument
argumentParser = argumentIdentifierParser <|> argumentPrimitiveParser

argumentsParser :: ParserOf st [Argument]
argumentsParser = argumentParser `sepBy` (spaces)

prefixExpressionParser :: ParserOf st Expression
prefixExpressionParser = do
  fnName <- identifierNameParser
  args   <- argumentsParser
  return $ Expression fnName args

infixExpressionParser :: ParserOf st Expression
infixExpressionParser = do
  arg1   <- argumentParser
  fnName <- identifierNameParser
  arg2   <- argumentParser
  return $ Expression fnName [arg1, arg2]

expressionParser :: ParserOf st Expression
expressionParser = prefixExpressionParser <|> infixExpressionParser

functionDefinitionParser :: ParserOf st FunctionDef
functionDefinitionParser = do
  sources <- identifiersParser
  _       <- spaces
  arrow   <- string "->"
  _       <- spaces
  sink    <- identifierParser
  return $ FunctionDef sources sink