module Main where

import Data.Either (fromRight, fromLeft)

import Types ( Type(..), LineGroup(..), AST(..), Context, mkContext)
import Parser ( programHeader, lineGroupParser, testParser )
import AST ( resolveTypes, mkPrimOpsContext, inheritContext )
import PrimOps ( primOps )
import PrimTypes ( integer )
import Transform ( fromAST )
import System.IO.Error (userError)

main :: IO ()
main = compile "./examples/fib.par"

compile :: FilePath -> IO ()
compile f = do
  putStrLn $ "Parsing file: " ++ f
  let ctx = inheritContext (mkPrimOpsContext primOps) (mkContext [ ("stdin", VariableType "stream")
                                                                 , ("main", NestedType "Function" [VariableType "stream", VariableType "stream"])
                                                                 ])
  putStrLn "Default context:"
  putStrLn $ show ctx
  putStrLn ""
  code <- readFile f
  putStrLn "Program code:"
  putStrLn code
  putStrLn ""
  let parseTrees = testParser lineGroupParser code
  putStrLn $ "Parse error: " ++ (showLeft $ parseTrees)
  let maybeLg = (either (const Nothing) (\lgs -> Just $ programHeader lgs) parseTrees)
  putStrLn "Parse tree:"
  putStrLn $ showJust maybeLg
  putStrLn ""
  let maybeEitherAst = fmap (\lg -> resolveTypes ctx lg) maybeLg
  putStrLn $ "AST error: " ++ (showJustLeft maybeEitherAst)
  let maybeAst = fmap (either (const Nothing) (\ast -> Just ast)) maybeEitherAst
  putStrLn "AST:"
  putStrLn $ showJust maybeAst
  putStrLn ""
  --llvmOut <- fmap (fromAST) maybeAst
  --putStrLn $ show llvmOut

showJust :: (Show a) => Maybe a -> String
showJust = maybe "" (\e -> show e)

showLeft :: (Show a, Show b) => Either a b -> String
showLeft = either (\v -> show v) (\_ -> "")

showJustLeft :: (Show a, Show b) => Maybe (Either a b) -> String
showJustLeft = maybe "" (\e -> showLeft e)
