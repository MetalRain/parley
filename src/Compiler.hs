module Compiler
  ( compile
  , wasSuccessful
  , CompileResult(..)
  ) where

import Data.Either (fromRight, fromLeft)
import Data.Maybe (fromJust, isNothing, isJust)

import Types ( Type(..), LineGroup(..), AST(..), Context, mkContext)
import Parser ( programHeader, lineGroupParser, testParser )
import AST ( typeCheck, mkPrimOpsContext, inheritContext, TypeError )
import PrimOps ( primOps )
import PrimTypes ( integer )
import Transform ( fromAST )
import PrettyShow ( indentRows )
import Text.Parsec.Error

defaultContext :: Context
defaultContext = inheritContext
  (mkPrimOpsContext primOps)
  (mkContext [ ("stdin", VariableType "stream")
             , ("main", NestedType "Function" [VariableType "stream", VariableType "stream"])
             ])

data CompileResult = CompileResult (Maybe LineGroup) (Maybe ParseError) (Maybe AST) (Maybe TypeError)

compile :: String -> CompileResult
compile code = CompileResult parseTree parseError ast typeError where
  parseTrees :: Either ParseError [LineGroup]
  parseTrees = testParser lineGroupParser code

  parseError :: Maybe ParseError
  parseError = either (\e -> Just e) (\_ -> Nothing) parseTrees

  parseTree :: Maybe LineGroup
  parseTree = (either (const Nothing) (\lgs -> Just $ programHeader lgs) parseTrees)

  maybeEitherAst :: Maybe (Either TypeError AST)
  maybeEitherAst = fmap (\lg -> typeCheck defaultContext lg) parseTree

  typeError :: Maybe TypeError
  typeError = fromJust $ fmap (either (\e -> Just e) (const Nothing)) maybeEitherAst

  ast :: Maybe AST
  ast = fromJust $ fmap (either (const Nothing) (\ast -> Just ast)) maybeEitherAst

wasSuccessful :: CompileResult -> Bool
wasSuccessful (CompileResult lg pe ast te) = (isJust lg) && (isNothing pe) && (isJust ast) && (isNothing te)