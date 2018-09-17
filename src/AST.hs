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
primType _ (PrimVector (TVector size [])) = Right $ vector 0 AnyType
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
  res = if (length errors) == 0 then Right $ typeType (fromRight AnyType fnType)
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
inheritContext c new = Map.unionWith (\lv rv -> rv) c new

mkPrimOpsContext :: [Expression] -> Context
mkPrimOpsContext primOps = Map.fromList pairs where
  pairs = map (\op@(NativeExpression name inT outT ) -> (name, function (inT ++ [outT]))) primOps

resolveTypes :: Context -> LineGroup -> Either TypeError AST
resolveTypes c lg@(LineGroup _ a lgs) = (either
  (\e -> Left $ enhanceErrorTrace e a)
  (\t -> resolveAssignment c t a lgs)
  (assignmentType c a))

resolveAssignment :: Context -> Type -> Assignment -> [LineGroup] -> Either TypeError AST
resolveAssignment oldC t a lgs = res where
  res = if ((length childErrors) == 0) then Right $ AST a newC (rights children)
                                       else Left $ enhanceErrorTrace (head childErrors) a
  newC = inheritContext oldC (assignmentContext t a)
  children = map (\lg -> resolveTypes newC lg) lgs
  childErrors = lefts children

assignmentContext :: Type -> Assignment -> Context
assignmentContext t (ExprAssign i _) = Map.fromList [(i, t)]
assignmentContext t (PrimAssign i _) = Map.fromList [(i, t)]


enhanceErrorTrace :: TypeError -> Assignment -> TypeError
enhanceErrorTrace (TypeError as e m) a = TypeError (a : as) e m