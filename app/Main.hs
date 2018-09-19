module Main where

import Data.Either (fromRight)

import Types ( Type(..), LineGroup(..), AST(..), Context, mkContext)
import Parser ( programHeader, lineGroupParser, testParser )
import AST ( resolveTypes, mkPrimOpsContext, inheritContext )
import PrimOps ( primOps )
import PrimTypes ( integer )
import Transform ( fromAST )
import System.IO.Error (userError)

main :: IO ()
main = compile "./examples/plus.par"

compile :: FilePath -> IO ()
compile f = do
  putStrLn $ "Parsing file: " ++ f
  let ctx = inheritContext (mkPrimOpsContext primOps) (mkContext [("stdin", UnresolvedType), ("main", UnresolvedType)])
  putStrLn "Default context:"
  putStrLn $ show ctx
  putStrLn ""
  code <- readFile f
  putStrLn "Program code:"
  putStrLn code
  putStrLn ""
  let parseTrees = testParser lineGroupParser code
  let maybeLg = (either (const Nothing) (\lgs -> Just $ programHeader lgs) parseTrees)
  putStrLn "Parse tree:"
  putStrLn $ show maybeLg
  putStrLn ""
  let eitherAst = fmap (\lg -> resolveTypes ctx lg) maybeLg
  let maybeAst = fmap (either (const Nothing) (\ast -> Just ast)) eitherAst
  putStrLn "AST:"
  putStrLn $ show maybeAst
  putStrLn ""
  --llvmOut <- fmap (fromAST) maybeAst
  --putStrLn $ show llvmOut

