{-# LANGUAGE FlexibleContexts #-}

module Parser
    ( identifierNameParser
    , typeNameParser
    , typeParser
    , identifierParser
    , integerParser
    , scalarParser
    , vectorParser
    , expressionParser
    , functionParser
    , assignmentParser
    , linesParser
    , lineGroupParser
    , testParser
    , mkScalar
    , programHeader
    ) where

import Types
    ( TypeName
    , IdentifierName
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
    , Indentation(..)
    , Line(..)
    , LineGroup(..)
    )

import Text.Parsec.Char ( alphaNum, anyChar, char, digit, endOfLine, lower, newline, noneOf, oneOf, spaces, string, upper )
import Text.Parsec.Prim ( Parsec(..), (<|>), many, parse, try )
import Text.Parsec.Combinator ( eof, lookAhead, many1, manyTill, notFollowedBy, option, parserTrace, parserTraced, sepBy, sepBy1 )

-- Utils
type ParserOf st a = Parsec String st a

testParser p = parse p ""

trailingSpace :: ParserOf st a -> ParserOf st a
trailingSpace p = do
  r <- p
  _ <- many (oneOf " ")
  return r

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

simpleTypeParser :: ParserOf st Type
simpleTypeParser = do
  name <- trailingSpace typeNameParser
  return $ Type name

dataTypeParser :: ParserOf st Type
dataTypeParser = do
  prim <- primitiveParser
  return $ DataType prim

nestedTypeParser :: ParserOf st Type
nestedTypeParser = do
  name     <- typeNameParser
  _        <- string "("
  subTypes <- (trailingSpace typeParser) `sepBy` (trailingSpace (string ","))
  _        <- trailingSpace (string ")")
  return $ NestedType name subTypes

variableTypeParser :: ParserOf st Type
variableTypeParser = do
  name <- trailingSpace (identifierNameParser)
  return $ VariableType name

typeParser :: ParserOf st Type
typeParser = (try nestedTypeParser)
           <|> (try dataTypeParser)
           <|> (try variableTypeParser)
           <|> simpleTypeParser

identifierParser :: ParserOf st Identifier
identifierParser = do
  idName   <- trailingSpace identifierNameParser
  _        <- trailingSpace (string ":")
  idType   <- typeParser
  return $ Identifier idName idType

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
  return $ mkScalar numerator denominator

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

functionParser :: ParserOf st TFunction
functionParser = do
  sources <- many (identifierParser)
  _       <- trailingSpace (string "->")
  expr    <- trailingSpace expressionParser
  return $ TFunction sources expr

functionPrimParser :: ParserOf st Primitive
functionPrimParser = PrimFunc <$> (trailingSpace functionParser)


argumentIdentifierParser :: ParserOf st Argument
argumentIdentifierParser = ArgIdent <$> (trailingSpace identifierNameParser)

argumentPrimitiveParser :: ParserOf st Argument
argumentPrimitiveParser = ArgPrim <$> (trailingSpace primitiveParser)


argumentParser :: ParserOf st Argument
argumentParser = (try argumentIdentifierParser)
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
expressionParser = (try prefixExpressionParser) <|> infixExpressionParser


primAssignParser :: ParserOf st Assignment
primAssignParser = do
  ident <- trailingSpace identifierNameParser
  _     <- trailingSpace (string "=")
  prim  <- trailingSpace primitiveParser
  return $ PrimAssign ident prim

exprAssignParser :: ParserOf st Assignment
exprAssignParser = do
  ident <- trailingSpace identifierNameParser
  _     <- trailingSpace (string "<-")
  expr  <- trailingSpace expressionParser
  return $ ExprAssign ident expr

typeAssignParser :: ParserOf st Assignment
typeAssignParser = do
  _       <- trailingSpace (string "alias")
  alias   <- typeParser
  _       <- trailingSpace (string "=")
  aliased <- typeParser
  return $ TypeAssign alias aliased

assignmentParser :: ParserOf st Assignment
assignmentParser = (try typeAssignParser)
                 <|> (try primAssignParser)
                 <|> exprAssignParser

assignmentLineParser :: ParserOf st Line
assignmentLineParser = do
  indent     <- many (char ' ')
  assignment <- trailingSpace assignmentParser
  return $ Line (toInteger $ length indent) assignment

commentLineParser :: ParserOf st Line
commentLineParser = do
  indent  <- many (char ' ')
  _       <- string "#"
  comment <- many (noneOf "\n")
  return $ CommentLine (toInteger $ length indent) comment

emptyLineParser :: ParserOf st Line
emptyLineParser = do
  _  <- many (noneOf "\n")
  return $ CommentLine 0 ""

lineParser :: ParserOf st Line
lineParser = (try commentLineParser)
           <|> (try assignmentLineParser)
           <|> emptyLineParser

linesParser :: ParserOf st [Line]
linesParser = do
  lines <- lineParser `sepBy` endOfLine
  _     <- eof
  return lines

isLeafer :: Indentation -> Line -> Bool
isLeafer currentLevel (Line nextLevel _) = currentLevel <= nextLevel

buildLineGroup :: LineGroup -> [Line] -> [LineGroup]
buildLineGroup current@(LineGroup level a children) tail@(next@(Line nextLevel b):rest)
  -- next is closer to root, current is leaf
  | level > nextLevel = [ current ]
  -- next is same level, continue building LineGroup
  | level == nextLevel = ( current : buildLineGroup (LineGroup nextLevel b []) rest)
  -- next is child of current, take rest until it's not leaf anymore
  | level < nextLevel = [LineGroup level a $ children ++ (buildLineGroup (LineGroup nextLevel b []) (takeWhile (isLeafer level) rest))]
-- comments are left out
buildLineGroup current@(LineGroup level a children) tail@(next@(CommentLine nextLevel b):rest) = buildLineGroup current rest
-- no more lines, current is leaf
buildLineGroup current@(LineGroup _ _ _) [] = [ current ]

notCommentLine :: Line -> Bool
notCommentLine (Line _ _) = True
notCommentLine (CommentLine _ _ ) = False

lineGroupParser :: ParserOf st [LineGroup]
lineGroupParser = do
  lines <- linesParser
  let ((Line level a):rest) = filter notCommentLine lines
  return $ buildLineGroup (LineGroup level a []) rest

programHeader :: [LineGroup] -> LineGroup
programHeader lgs = LineGroup 0 (ExprAssign "stdout" (Expression "main" [ArgIdent "stdin"])) lgs