{-# LANGUAGE NamedFieldPuns #-}
module Typing.TypeContext(
    Context,
    add,
    find,
    toplevelTyping,
    typeOfDesc,
    findByLabels,
    rightMostType,
    typeOfTexp,
    typeOfTpat
) where


import qualified Parsing.AST as AST
import qualified Data.Map as Map
import qualified Typing.TypedTree as Typed
import Data.List (groupBy)
import qualified Typing.Builtin as Builtin
import Parsing.AST (TypeData(typeDataName))
import Data.Maybe


typeOfTexp :: Typed.Texp -> Typed.Type
typeOfTexp (Typed.Var _ ty) = ty
typeOfTexp (Typed.Abs _ _ ty) = ty
typeOfTexp (Typed.Apply _ _ ty) = ty
typeOfTexp (Typed.Let _ _ _ ty) = ty
typeOfTexp (Typed.Letrec _ _ _ ty) = ty
typeOfTexp (Typed.If _ _ _ ty) = ty
typeOfTexp (Typed.Match _ _ ty) = ty
typeOfTexp (Typed.Prim _ _ ty) = ty
typeOfTexp (Typed.Tuple _ ty) = ty
typeOfTexp (Typed.Record _ _ ty) = ty
typeOfTexp (Typed.Constant _ ty) = ty
typeOfTexp (Typed.Hole ty) = ty
typeOfTexp (Typed.Error ty) = ty

typeOfTpat :: Typed.Pattern -> Typed.Type
typeOfTpat (Typed.PatVar _ ty) = ty
typeOfTpat (Typed.PatTuple _ ty) = ty
typeOfTpat (Typed.PatRecord _ _ _ ty) = ty
typeOfTpat (Typed.PatConstruct _ ty) = ty
typeOfTpat (Typed.PatConstraint _ _ ty) = ty 
typeOfTpat (Typed.PatConstant _ ty) = ty
typeOfTpat (Typed.PatHole ty) = ty


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

toplevelTyping :: AST.Toplevel -> Context
toplevelTyping (AST.Toplevel types bindings) =
    Context {
        recordTypes = Map.empty,
        types = Map.fromList ((\x -> (AST.typeDataName x, typeOfDesc $ AST.typeDataDesc x)) <$> types),
        bindings = Map.fromList ((\x -> (AST.bindingName x, typeOfDesc $ AST.bindingType x)) <$> bindings)
    }

typeOfDesc :: AST.TypeDesc -> Typed.Type
typeOfDesc (AST.TypeDescVar x) = Typed.TypeVar x
typeOfDesc (AST.TypeDescArrow x y) = Typed.TypeArrow (typeOfDesc x) (typeOfDesc y)
typeOfDesc (AST.TypeDescTuple elems) = Typed.TypeTuple (map typeOfDesc elems)
typeOfDesc (AST.TypeDescRecord labels descs) =
    Typed.TypeRecord labels (map typeOfDesc descs)
typeOfDesc (AST.TypeDescTaggedUnion unions) =
    let (tags,descs) = unzip unions
    in Typed.TypeTaggedUnion (zip tags (map typeOfDesc descs))
typeOfDesc _ = error "ababab"


rightMostType :: Typed.Type -> Typed.Type
rightMostType (Typed.TypeArrow _ r) = rightMostType r
rightMostType x = x

