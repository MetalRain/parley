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

type Message = String
data TypeError = TypeError [Assignment] Expression Message deriving (Eq)
instance Show TypeError where
  show (TypeError as e m) = "Type error:" ++ m ++ "\n  in expression: " ++ (show e) ++ "\n  stack:\n    " ++ (intercalate "\n    " $ (map show as))

type Typing = Either TypeError Type


notDefined :: Context -> Expression -> IdentifierName -> TypeError
notDefined c e n = TypeError [] e $ (show n) ++ " not found in context " ++ (show c)

typeMismatchError :: Context -> Expression -> [Type] -> [Type] -> TypeError
typeMismatchError c e xs ys = TypeError [] e $ (show xs) ++ " don't match with " ++ (show ys) ++ " in context: \n" ++ (show c)

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

typeType :: Type -> Type
 -- Function is nested type where return type is last type
typeType (NestedType "Function" ts) = last ts
typeType t = t

exprType :: Context -> Expression -> Typing
exprType c (NativeExpression _ _ out) = Right out
exprType c e@(Expression n args) = res where
  -- check that function & args have types
  res = if (length errors) == 0 then Right $ typeType fnType
                                else Left $ head errors
  errors = lefts $ (fnTypeE : argTypesE) ++ mismatchErrors
  fnTypeE = identNameType c e n
  fnType = (fromRight UnresolvedType fnTypeE)
  argTypesE = map (\a -> argType c e a) args
  argTypes = rights argTypesE
  -- check that types match
  mismatchErrors = case fnType of
    (NestedType "Function" fnArgTypes) -> (matchManyTypes c e) (take (length argTypes) fnArgTypes) argTypes
    _                                  -> [ Left $ typeMismatchError c e [NestedType "Function" argTypes] [fnType] ]

matchManyTypes :: Context -> Expression -> [Type] -> [Type] -> [Typing]
matchManyTypes c e xs ys = res where
  res = if (length xs) /= (length ys) then [ Left $ typeMismatchError c e xs ys ]
                                      else zipWith (matchTypes c e) xs ys

matchTypes :: Context -> Expression -> Type -> Type -> Typing
matchTypes c e nt@(NestedType n ns) mt@(NestedType m ms) = res where
  -- match names & subtypes of nested types
  res = if n == m then (if length errors > 0 then Left $ head errors
                                             else Right nt)
                  else Left $ typeMismatchError c e [NestedType n []] [NestedType m []]
  errors = lefts matchesE
  matchesE = matchManyTypes c e ns ms
-- Variable types can bind to anything
-- TODO: only bind once
matchTypes _ _ (VariableType _) t = Right t
matchTypes _ _ t (VariableType _) = Right t
-- Strict comparison, must be same type
matchTypes c e expected actual = if expected == actual then Right expected
                                                       else Left $ typeMismatchError c e [expected] [actual]

argType :: Context -> Expression -> Argument -> Typing
argType c e (ArgIdent n) = identNameType c e n
argType c e (ArgPrim p) = primType c p

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

resolveChildContext :: Context -> Assignment -> [LineGroup] -> Either TypeError Context
resolveChildContext c a lgs = res where
  childTypeStubs = map (\(LineGroup _ a _) -> (assignmentIdentifierName a, UnresolvedType)) lgs
  ident = assignmentIdentifierName a
  newContext = fillContext c $ mkContext $ [(ident, UnresolvedType)] ++ childTypeStubs ++ (functionArgumentTypeStubs a)
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

enhanceErrorTrace :: TypeError -> Assignment -> TypeError
enhanceErrorTrace (TypeError as e m) a = TypeError (a : as) e m