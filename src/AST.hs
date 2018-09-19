module AST
    ( identType
    , primType
    , exprType
    , resolveTypes
    , inheritContext
    , TypeError(..)
    , Message
    , notDefined
    , enhanceErrorTrace
    , mkPrimOpsContext
    ) where

import Data.Either
import Data.Maybe
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
  , Context
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
data TypeError = TypeError [Assignment] Expression Message deriving (Show, Eq)

type Typing = Either TypeError Type


notDefined :: Context -> Expression -> IdentifierName -> TypeError
notDefined c e n = TypeError [] e $ (show n) ++ " not found in context " ++ (show c)


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
  res = if (length errors) == 0 then Right $ typeType (fromRight UnresolvedType fnType)
                                else head errors
  errors = filter (\t -> isLeft t) (fnType : argTypes)
  fnType = identNameType c e n
  argTypes = map (\a -> argType c e a) args


argType :: Context -> Expression -> Argument -> Typing
argType c e (ArgIdent n) = identNameType c e n
argType c e (ArgPrim p) = primType c p


contextLookup :: Context -> IdentifierName -> Maybe Type
contextLookup c n = Map.lookup n c

assignmentType :: Context -> Assignment -> Typing
assignmentType c (ExprAssign _ e) = exprType c e
assignmentType c (PrimAssign _ p) = primType c p

inheritContext :: Context -> Context -> Context
inheritContext c new = Map.unionWith overrideType c new

fillContext :: Context -> Context -> Context
fillContext c new = Map.unionWith fillType c new

overrideType :: Type -> Type -> Type
overrideType a b = b

fillType :: Type -> Type -> Type
fillType a UnresolvedType = a
fillType UnresolvedType b = b
fillType _ b = b

mkPrimOpsContext :: [Expression] -> Context
mkPrimOpsContext primOps = Map.fromList pairs where
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