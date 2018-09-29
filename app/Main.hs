module Main where

import System.Environment
import Compiler
import Data.Maybe
import Control.Monad.IO.Class

main :: IO ()
main = do
  args <- getArgs
  code <- readFile (head args)
  putStrLn $ "Code:\n" ++ code
  let (CompileResult parseTree parseError ast typeError) = compile code
  if isJust parseTree then liftIO $ putStrLn $ "Parse tree:\n" ++ showJust parseTree
                      else liftIO $ putStrLn $ "Parse error:\n" ++ showJust parseError
  if isJust ast then liftIO $ putStrLn $ "AST:\n" ++ showJust ast
                else liftIO $ putStrLn $ showJust typeError 

showJust :: (Show a) => Maybe a -> String
showJust = maybe "" (\e -> show e)