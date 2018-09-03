{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( identifierNameParser
    , typeNameParser
    , identifierParser
    , integerParser
    , testParser
    , Type(..)
    , Identifier(..)
    ) where

import Data.Char (isSpace)
import Text.Parsec.Char ( alphaNum, char, digit, lower, oneOf, spaces, string, upper )
import Text.Parsec.Prim ( (<|>), Parsec(..), parse, many )
import Text.Parsec.Combinator ( notFollowedBy, many1, option, sepBy, sepBy1 )

type TypeName = String
type IdentifierName = String
type FunctionName = IdentifierName

data Type = Type TypeName deriving (Eq, Show)
type InputType = Type
type OutputType = Type

-- Primary types
type TInteger = Integer
type TScalar = Rational
data TVector = TVector TInteger Type deriving (Eq, Show)

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


testParser p = parse p ""

identifierNameParser :: Parsec String st IdentifierName
identifierNameParser = do
  start <- lower
  rest  <- many (alphaNum)
  return $ (start:rest)

typeNameParser :: Parsec String st TypeName
typeNameParser = do
  start <- upper
  rest  <- many (alphaNum)
  return $ (start:rest)

identifierParser :: Parsec String st Identifier
identifierParser = do
  idName   <- identifierNameParser
  _        <- spaces
  colon    <- string ":"
  _        <- spaces
  typeName <- typeNameParser
  return $ Identifier idName (Type typeName)

identifiersParser :: Parsec String st [Identifier]
identifiersParser = identifierParser `sepBy` (spaces)


negIntegerParser :: Parsec String st TInteger
negIntegerParser = do
  sign <- char '-'
  number <- oneOf "123456789"
  moreNumbers <- many (digit)
  return $ read $ [sign, number] ++ moreNumbers

zeroIntegerParser :: Parsec String st TInteger
zeroIntegerParser = do
  zero <- char '0'
  notFollowedBy digit
  return 0

posIntegerParser :: Parsec String st TInteger
posIntegerParser = do
  number <- oneOf "123456789"
  moreNumbers <- many (digit)
  return $ read $ [number] ++ moreNumbers

integerParser :: Parsec String st TInteger
integerParser = negIntegerParser <|> zeroIntegerParser <|> posIntegerParser

integerPrimParser :: Parsec String st Primitive
integerPrimParser = PrimInt <$> integerParser

--scalarParser :: Parsec String st TScalar
--scalarParser = _

primitiveParser :: Parsec String st Primitive
primitiveParser = integerPrimParser


argumentIdentifierParser :: Parsec String st Argument
argumentIdentifierParser = do
  ident <- identifierNameParser
  return $ ArgIdent ident


argumentPrimitiveParser :: Parsec String st Argument
argumentPrimitiveParser = do
  prim <- primitiveParser
  return $ ArgPrim prim


argumentParser :: Parsec String st Argument
argumentParser = argumentIdentifierParser <|> argumentPrimitiveParser

argumentsParser :: Parsec String st [Argument]
argumentsParser = argumentParser `sepBy` (spaces)

prefixExpressionParser :: Parsec String st Expression
prefixExpressionParser = do
  fnName <- identifierNameParser
  args   <- argumentsParser
  return $ Expression fnName args

infixExpressionParser :: Parsec String st Expression
infixExpressionParser = do
  arg1   <- argumentParser
  fnName <- identifierNameParser
  arg2   <- argumentParser
  return $ Expression fnName [arg1, arg2]

expressionParser :: Parsec String st Expression
expressionParser = prefixExpressionParser <|> infixExpressionParser

functionDefinitionParser :: Parsec String st FunctionDef
functionDefinitionParser = do
  sources <- identifiersParser
  _       <- spaces
  arrow   <- string "->"
  _       <- spaces
  sink    <- identifierParser
  return $ FunctionDef sources sink