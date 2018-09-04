{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( identifierNameParser
    , typeNameParser
    , identifierParser
    , integerParser
    , scalarParser
    , vectorParser
    , expressionParser
    , functionParser
    , assignmentParser
    , programParser
    , testParser
    , Type(..)
    , Identifier(..)
    , TInteger
    , TScalar
    , mkScalar
    , TVector(..)
    , TFunction(..)
    , Primitive(..)
    , Expression(..)
    , Argument(..)
    , Assignment(..)
    ) where

import Data.Ratio ( (%) )
import Text.Parsec.Char ( alphaNum, char, digit, endOfLine, lower, oneOf, spaces, string, upper )
import Text.Parsec.Prim ( (<|>), Parsec(..), many, parse, try )
import Text.Parsec.Combinator ( choice, eof, many1, notFollowedBy, option, sepBy, sepBy1 )

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

type Source = Identifier              
type Sink = Identifier
{-
Function definition:
IdentifierName = a: T b: T -> Expression
  Assignments 
-}
data TFunction = TFunction [Source] Expression Program deriving (Eq, Show)

-- Syntax
data Identifier = Identifier IdentifierName Type deriving (Eq, Show)
data Primitive = PrimInt TInteger
               | PrimScalar TScalar
               | PrimVector TVector
               | PrimFunc TFunction
                deriving (Eq, Show)
data Argument = ArgIdent IdentifierName
              | ArgPrim Primitive
               deriving (Eq, Show)


{- 
Expressions:
1 plus 2
f 1 2
f (1,2,3) a
-}
data Expression = Expression FunctionName [Argument] deriving (Eq, Show)


{-
Assignment:
a = 1
b = 2.1
c = (1, 2, 4)
d = a b -> a + b

Evaluation:
a <- Expression
-}
data Assignment = PrimAssign IdentifierName Primitive
                | ExprAssign IdentifierName Expression
                 deriving (Eq, Show)

type Program = [Assignment]



-- Utils

testParser p = parse p ""

trailingSpace :: ParserOf st a -> ParserOf st a
trailingSpace p = do
  r <- p
  _ <- many (oneOf " ")
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
  idName   <- trailingSpace identifierNameParser
  _        <- trailingSpace (string ":")
  typeName <- trailingSpace typeNameParser
  return $ Identifier idName (Type typeName)

negIntegerParser :: ParserOf st TInteger
negIntegerParser = do
  sign        <- char '-'
  number      <- oneOf "123456789"
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
  number      <- oneOf "123456789"
  moreNumbers <- many (digit)
  notFollowedBy digit
  return $ read $ [number] ++ moreNumbers

integerParser :: ParserOf st TInteger
integerParser = negIntegerParser <|> zeroIntegerParser <|> posIntegerParser

integerPrimParser :: ParserOf st Primitive
integerPrimParser = PrimInt <$> (trailingSpace integerParser)

scalarParser :: ParserOf st TScalar
scalarParser = do
  numerator   <- trailingSpace integerParser
  _           <- trailingSpace (string "/")
  denominator <- trailingSpace posIntegerParser
  return $ (fromIntegral numerator) % (fromIntegral denominator)

scalarPrimParser :: ParserOf st Primitive
scalarPrimParser = PrimScalar <$> (trailingSpace scalarParser)

primitiveParser :: ParserOf st Primitive
primitiveParser = (try functionPrimParser)
                <|> (try vectorPrimParser)
                <|> (try scalarPrimParser)
                <|> integerPrimParser

vectorParser :: ParserOf st TVector
vectorParser = do
  start <- char '('
  prims <- (trailingSpace primitiveParser) `sepBy` (trailingSpace (string ","))
  end   <- char ')'
  return $ TVector (toInteger (length prims)) prims

vectorPrimParser :: ParserOf st Primitive
vectorPrimParser = PrimVector <$> (trailingSpace vectorParser)


functionLineParser :: String -> ParserOf st Assignment
functionLineParser indent = do
  _      <- endOfLine
  _      <- string indent
  line   <- assignmentParser
  return line

functionBodyParser :: ParserOf st Program
functionBodyParser = do
  _      <- endOfLine
  indent <- many1 (char ' ')
  line   <- assignmentParser
  rest   <- many (try $ functionLineParser indent)
  return $ (line : rest)

functionParser :: ParserOf st TFunction
functionParser = do
  sources <- many (identifierParser)
  _       <- trailingSpace (string "->")
  expr    <- trailingSpace expressionParser
  body    <- option [] functionBodyParser
  return $ TFunction sources expr body

functionPrimParser :: ParserOf st Primitive
functionPrimParser = PrimFunc <$> (trailingSpace functionParser)


argumentIdentifierParser :: ParserOf st Argument
argumentIdentifierParser = ArgIdent <$> (trailingSpace identifierNameParser)

argumentPrimitiveParser :: ParserOf st Argument
argumentPrimitiveParser = ArgPrim <$> (trailingSpace primitiveParser)


argumentParser :: ParserOf st Argument
argumentParser = try argumentIdentifierParser
               <|> argumentPrimitiveParser

argumentsParser :: ParserOf st [Argument]
argumentsParser = many1 (trailingSpace argumentParser)

prefixExpressionParser :: ParserOf st Expression
prefixExpressionParser = do
  fnName <- trailingSpace identifierNameParser
  args   <- trailingSpace argumentsParser
  return $ Expression fnName args

infixExpressionParser :: ParserOf st Expression
infixExpressionParser = do
  arg1   <- trailingSpace argumentPrimitiveParser
  fnName <- trailingSpace identifierNameParser
  arg2   <- trailingSpace argumentPrimitiveParser
  return $ Expression fnName [arg1, arg2]

expressionParser :: ParserOf st Expression
expressionParser = try prefixExpressionParser
                 <|> infixExpressionParser


primAssignParser :: ParserOf st Assignment
primAssignParser = do
  ident <- trailingSpace identifierNameParser
  _     <- trailingSpace (string "=")
  prim  <- trailingSpace primitiveParser
  return $ PrimAssign ident prim

primExprParser :: ParserOf st Assignment
primExprParser = do
  ident <- trailingSpace identifierNameParser
  _     <- trailingSpace (string "<-")
  expr  <- trailingSpace expressionParser
  return $ ExprAssign ident expr

assignmentParser :: ParserOf st Assignment
assignmentParser = try primAssignParser 
                 <|> primExprParser                 

programParser :: ParserOf st Program
programParser = (trailingSpace assignmentParser) `sepBy1` endOfLine