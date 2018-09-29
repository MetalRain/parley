module PrettyShow
  ( mkIndent
  , indent
  , indentRows
  , showMany
  , showKeyValue
  , showIdentType
  , withSpaces
  , withRows
  , withCommas
  ) where

import Prelude hiding ( showList )
import Data.List ( intersperse, intercalate )

mkIndent :: Int -> String
mkIndent i = replicate i ' '

indent :: Int -> String -> String
indent i s = (mkIndent i) ++ s

indentRows :: Int -> String -> String
indentRows i s = unlines $ map (indent i) $ lines s

showMany :: (Show a) => [a] -> [String]
showMany xs = map show xs

showKeyValue :: (Show a) => (String, a) -> String
showKeyValue (k, v) = withSpaces [k, "=", show v]

showIdentType :: (Show a) => (String, a) -> String
showIdentType (k, v) = k ++ ": " ++ show v

withSpaces :: [String] -> String
withSpaces = intercalate " "

withRows :: [String] -> String
withRows = intercalate "\n"

withCommas :: [String] -> String
withCommas = intercalate ", "