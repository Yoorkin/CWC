{-# LANGUAGE NamedFieldPuns #-}
module Typing.TypeContext where


import qualified Parsing.AST as AST
import qualified Data.Map as Map
import qualified Typing.TypedTree as Typed
import Data.List (groupBy)
import qualified Typing.Builtin as Builtin
import Parsing.AST
import Data.Maybe
import qualified Control.Arrow as Typed


typeOfTexp :: Typed.Texp -> Typed.Type
typeOfTexp (Typed.Var _ ty) = ty
typeOfTexp (Typed.Abs _ _ ty) = ty
typeOfTexp (Typed.Apply _ _ ty) = ty
typeOfTexp (Typed.Let _ _ _ ty) = ty
typeOfTexp (Typed.Letrec _ _ _ ty) = ty
typeOfTexp (Typed.If _ _ _ ty) = ty
typeOfTexp (Typed.Match _ _ _ ty) = ty
typeOfTexp (Typed.Prim _ _ ty) = ty
typeOfTexp (Typed.Tuple _ ty) = ty
typeOfTexp (Typed.Constant _ ty) = ty
typeOfTexp (Typed.Hole ty) = ty
typeOfTexp (Typed.Error ty) = ty

updateTypeOfTexp :: Typed.Type -> Typed.Texp -> Typed.Texp 
updateTypeOfTexp ty expr = 
    case expr of
        (Typed.Var a _) -> Typed.Var a ty
        (Typed.Abs a b _) -> Typed.Abs a b ty
        (Typed.Apply a b _) -> Typed.Apply a b ty
        (Typed.Let a b c _) -> Typed.Let a b c ty
        (Typed.Letrec a b c _) -> Typed.Letrec a b c ty
        (Typed.If a b c _) -> Typed.If a b c ty
        (Typed.Match a b c _) -> Typed.Match a b c ty
        (Typed.Prim a b _) -> Typed.Prim a b ty
        (Typed.Tuple a _) -> Typed.Tuple a ty
        (Typed.Constant a _) -> Typed.Constant a ty
        (Typed.Hole _) -> Typed.Hole ty
        (Typed.Error _) -> Typed.Error ty


typeOfTpat :: Typed.Pattern -> Typed.Type
typeOfTpat (Typed.PatVar _ ty) = ty
typeOfTpat (Typed.PatTuple _ ty) = ty
typeOfTpat (Typed.PatConstr _ _ ty) = ty
typeOfTpat (Typed.PatConstant _ ty) = ty
typeOfTpat (Typed.PatHole ty) = ty

typeOfConst :: AST.Constant -> Typed.Type
typeOfConst c = case c of
        Integer _ -> Builtin.intType
        Boolean _ -> Builtin.boolType
        Float _ -> Builtin.floatType
        String _ -> Builtin.stringType
        Char _ -> Builtin.charType
        Unit -> Builtin.unitType

data Context = Context {
    recordTypes :: Map.Map [String] Typed.Type,
    types :: Map.Map String Typed.Type,
    bindings :: Map.Map String Typed.Type
}

add :: String -> Typed.Type -> Context -> Context
add name ty ctx = ctx { bindings = Map.insert name ty (bindings ctx) }


find :: String -> Context -> Maybe Typed.Type
find name Context{ bindings } = Map.lookup name bindings

findByLabels :: [String] -> Context -> Maybe Typed.Type
findByLabels ls Context{ recordTypes } = Map.lookup ls recordTypes


freshName :: Int -> String
freshName i = [l:show r | l <- ['a'..'z'], r <- [0..]] !! i

typeOfAnno :: AST.Annotation -> Typed.Type
typeOfAnno x = case x of
    (AST.AnnoVar x) -> Typed.TypeVar x
    (AST.AnnoArrow x y) -> Typed.TypeArrow (typeOfAnno x) (typeOfAnno y)
    (AST.AnnoTuple elems) -> Typed.TypeTuple (map typeOfAnno elems)
    (AST.AnnoTypeConstr scheme vars) -> Typed.TypeConstruction scheme (typeOfAnno vars)
    _ -> error "ababab"

-- rightMostType :: Typed.Type -> Typed.Type
-- rightMostType (Typed.TypeArrow _ r) = rightMostType r
-- rightMostType x = x

-- leftSeqType :: Typed.Type -> [Typed.Type]
-- leftSeqType (Typed.TypeArrow l r) = l : leftSeqType r  
-- leftSeqType _ = []

decomposeArrowType :: Typed.Type -> ([Typed.Type], Typed.Type)
decomposeArrowType ty@(Typed.TypeArrow _ _) = loop ty []
        where loop (Typed.TypeArrow l r) args = loop r (l : args)
              loop ret args = (args,ret) 
decomposeArrowType x = error $ show x ++ " is not a arrow type"

composeArrowType :: [Typed.Type] -> Typed.Type -> Typed.Type
composeArrowType (arg:args) ret = Typed.TypeArrow arg (composeArrowType args ret)
composeArrowType _ ret = ret

addPatternVars :: Typed.Pattern -> Context -> Context
addPatternVars pat ctx = 
    case pat of
        Typed.PatVar name ty -> add name ty ctx
        Typed.PatTuple elems _ -> loop elems ctx
        Typed.PatConstr _ pat' _  -> addPatternVars pat' ctx
        Typed.PatHole _ -> ctx
        Typed.PatError _ -> ctx
        Typed.PatConstant _ _ -> ctx
    where 
        loop [] ctx = ctx
        loop (x:xs) ctx = loop xs (addPatternVars x ctx)
