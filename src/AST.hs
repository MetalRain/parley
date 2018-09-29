module AST
    ( identType
    , primType
    , exprType
    , resolveTypes
    , inheritContext
    , TypeError(..)
    , Message
    , notDefined
    , typeMismatchError
    , enhanceErrorTrace
    , mkPrimOpsContext
    ) where

import Prelude hiding (showList)
import Data.Either
import Data.Maybe
import Data.List ( intercalate )
import qualified Data.Map.Strict as Map
import Types
  ( AST(..)
  , LineGroup(..)
  , Context
  , Assignment(..)
  , Identifier(..)
  , Type(..)
  , TFunction(..)
  , TVector(..)
  , Primitive(..)
  , Argument(..)
  , Expression(..)
  , Context(..)
  , IdentifierName
  , mkContext
  )
import PrimTypes
  ( integer
  , scalar
  , vector
  , function
  )
import PrettyShow
  ( showMany
  , withCommas
  , withSpaces
  , withRows
  , indent
  , indentRows
  )

-- Type errors

type Message = String
data TypeError = TypeError Context [Assignment] Expression Message deriving (Eq)
instance Show TypeError where
  show (TypeError c as e m) = withRows [ "Type error:"
                                       , indent 4 m
                                       , indent 2 "expression:"
                                       , indent 4 (show e)
                                       , indent 2 "stack:"
                                       , indentRows 4 (withRows $ showMany as)
                                       , indent 2 "context:"
                                       , indentRows 4 (show c)
                                       ]

type Typing = Either TypeError Type


notDefined :: Context -> Expression -> IdentifierName -> TypeError
notDefined c e n = TypeError c [] e $ (show n) ++ " not defined"

typeMismatchError :: String -> Context -> Expression -> [Type] -> [Type] -> TypeError
typeMismatchError m c e xs ys = TypeError c [] e $ withSpaces [ withCommas $ showMany xs
                                                              , "don't match with"
                                                              , withCommas $ showMany ys
                                                              , "in"
                                                              , m
                                                              ]

enhanceErrorTrace :: TypeError -> Assignment -> TypeError
enhanceErrorTrace (TypeError c as e m) a = TypeError c (a : as) e m

-- Type resolving

identType :: Identifier -> Type
identType (Identifier _ t) = t

identNameType :: Context -> Expression -> IdentifierName -> Typing
identNameType c e n = maybe (Left $ notDefined c e n) (\t -> Right t) (contextLookup c n)

primType :: Context -> Primitive -> Typing
primType _ (PrimInt _)    = Right integer
primType _ (PrimScalar _) = Right scalar
primType _ (PrimVector (TVector size [])) = Right $ vector 0 UnresolvedType
primType c (PrimVector (TVector size ps)) = (either
  (Left)
  (\t -> Right $ vector size t)
  (head $ map (\p -> primType c p) ps))
primType c (PrimFunc (TFunction args e)) = (either
  (Left)
  (\t -> Right $ function $ (map identType args) ++ [t])
  (exprType c e))

typeType :: Context -> Type -> Typing
 -- Function is nested type where return type is last type
typeType _ (NestedType "Function" ts) = Right $ last ts
typeType c t@(VariableType n) = Right $ fromMaybe t $ contextLookup c n
typeType c t@(DataType p) = primType c p
typeType _ t = Right t


mkResType :: IdentifierName -> Type
mkResType n = VariableType (n ++ "Result")

exprType :: Context -> Expression -> Typing
exprType c (NativeExpression _ _ out) = Right out
exprType c e@(Expression n args) = res where
  -- check that function & args have types
  res = if (length errors) == 0 then typeType c fnType
                                else Left $ head errors
  errors = lefts $ [fnTypeE] ++ argTypesE ++ mismatchErrors
  fnTypeE = identNameType c e n
  fnType = (fromRight UnresolvedType fnTypeE)
  argTypesE = map (\a -> argType c e a) args
  argTypes = rights argTypesE
  -- check that types match
  mismatchErrors = case fnType of
    (NestedType "Function" fnArgTypes) -> (matchManyTypes c e) (take (length argTypes) fnArgTypes) argTypes
    UnresolvedType                     -> []
    _                                  -> [ Left $ typeMismatchError "expression" c e [NestedType "Function" argTypes] [fnType] ]

matchManyTypes :: Context -> Expression -> [Type] -> [Type] -> [Typing]
matchManyTypes c e xs ys = res where
  res = if (length xs) /= (length ys) then [ Left $ typeMismatchError "function arguments" c e xs ys ]
                                      else zipWith (matchTypes c e) xs ys

matchTypes :: Context -> Expression -> Type -> Type -> Typing
matchTypes c e nt@(NestedType n ns) mt@(NestedType m ms) = res where
  -- match names & subtypes of nested types
  res = if n == m then (if length errors > 0 then Left $ head errors
                                             else Right nt)
                  else Left $ typeMismatchError "function name" c e [NestedType n []] [NestedType m []]
  errors = lefts matchesE
  matchesE = matchManyTypes c e ns ms
-- Variable types can bind to anything, but prefer variable over unresolved
matchTypes _ _ (VariableType _) t = Right t
matchTypes _ _ t (VariableType _) = Right t
-- Unresolved types can be anything 
matchTypes _ _ t UnresolvedType = Right t
matchTypes _ _ UnresolvedType t = Right t
-- Strict comparison, must be same type
matchTypes c e expected actual = if expected == actual then Right expected
                                                       else Left $ typeMismatchError "type" c e [expected] [actual]

argType :: Context -> Expression -> Argument -> Typing
argType c e (ArgIdent n) = identNameType c e n
argType c e (ArgPrim p) = primType c p

resolveTypes :: Context -> LineGroup -> Either TypeError AST
resolveTypes c lg@(LineGroup _ a lgs) = 
  (either
  (\e -> Left $ enhanceErrorTrace e a)
  (\newC -> resolveChildTypes newC a lgs)
  (resolveChildContext c a lgs))

resolveChildTypes :: Context -> Assignment -> [LineGroup] -> Either TypeError AST
resolveChildTypes c a lgs = res where
  children = map (\lg -> resolveTypes c lg) lgs
  childErrors = lefts children
  res = if ((length childErrors) == 0) then Right $ AST a c (rights children)
                                       else Left $ enhanceErrorTrace (head childErrors) a

-- Context handling

contextLookup :: Context -> IdentifierName -> Maybe Type
contextLookup (Context m) n = Map.lookup n m

assignmentType :: Context -> Assignment -> Typing
assignmentType c (ExprAssign _ e) = exprType c e
assignmentType c (PrimAssign _ p) = primType c p

inheritContext :: Context -> Context -> Context
inheritContext (Context m) (Context newM) = Context $ Map.unionWith overrideType m newM

fillContext :: Context -> Context -> Context
fillContext (Context m) (Context newM) = Context $ Map.unionWith fillType m newM

overrideType :: Type -> Type -> Type
overrideType a b = b

fillType :: Type -> Type -> Type
fillType a UnresolvedType = a
fillType UnresolvedType b = b
fillType _ b = b

mkPrimOpsContext :: [Expression] -> Context
mkPrimOpsContext primOps = Context $ Map.fromList pairs where
  pairs = map (\op@(NativeExpression name inT outT ) -> (name, function (inT ++ [outT]))) primOps

assignmentStubContext :: Assignment -> (IdentifierName, Type)
assignmentStubContext (PrimAssign ident p) = (ident, UnresolvedType)
assignmentStubContext (ExprAssign ident e) = (ident, UnresolvedType)

resolveChildContext :: Context -> Assignment -> [LineGroup] -> Either TypeError Context
resolveChildContext c a lgs = res where
  childTypeStubs = map (\(LineGroup _ a _) -> assignmentStubContext a) lgs
  ident = assignmentIdentifierName a
  newContext = fillContext c $ mkContext $ [assignmentStubContext a] ++ childTypeStubs ++ (functionArgumentTypeStubs a)
  -- Resolve assignment type when child identifiers are stubbed
  res = (either
    (\e -> Left e)
    (\t -> Right $ inheritContext newContext $ mkContext [(ident, t)] )
    (assignmentType newContext a))

-- Function arguments define context types for expression in function
functionArgumentTypeStubs :: Assignment -> [(IdentifierName, Type)]
functionArgumentTypeStubs (PrimAssign _ (PrimFunc (TFunction args _ ))) = map (\(Identifier i t) -> (i, t)) args
functionArgumentTypeStubs _ = []

assignmentIdentifierName :: Assignment -> IdentifierName
assignmentIdentifierName (ExprAssign i _) = i
assignmentIdentifierName (PrimAssign i _) = i