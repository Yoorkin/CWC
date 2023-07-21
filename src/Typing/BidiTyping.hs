{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Typing.BidiTyping(checkType,inferType,checkToplevelType) where

import qualified Parsing.AST as AST
import qualified Typing.TypedTree as Typed
import qualified Typing.TypeContext as Context
-- import qualified Data.Map as Map
import qualified Typing.Builtin as Builtin
import qualified Control.Monad.Writer as Writer
import Data.Functor(($>))
import Control.Monad (zipWithM)
import Parsing.AST (Binding(bindingType))

type W a = Writer.Writer [String] a

-- data Diagnostic
--     = TypeMismatch { expectedType :: Typed.Type, encountedType :: Typed.Type }

checkType :: Context.Context -> AST.Mexp -> Typed.Type -> W Typed.Texp
inferType :: Context.Context -> AST.Mexp -> W Typed.Texp

checkType ctx (AST.Abs (AST.PatVar x) m) ty@(Typed.TypeArrow t1 t2) = do
    m' <- checkType (Context.add x t1 ctx) m t2
    pure $ Typed.Abs (Typed.PatVar x t1) m' ty

checkType ctx (AST.If cond ifso ifnot) t = do
    cond' <- checkType ctx cond Builtin.boolType
    ifso' <- checkType ctx ifso t
    ifnot' <- checkType ctx ifnot t
    pure $ Typed.If cond' ifso' ifnot' t

checkType ctx (AST.Tuple vs) ty@(Typed.TypeTuple tys) =
    if length vs == length tys
        then Typed.Tuple <$> zipWithM (checkType ctx) vs tys <*> pure ty
        else Writer.tell ["tuple size is not " ++ show (length tys)]
             $> Typed.Error ty

checkType ctx (AST.Match cond cases) ty =
    let (caseConds, caseTerms) = unzip cases
    in do
        cond' <- inferType ctx cond
        caseConds' <- zipWithM (checkPatternType ctx) caseConds (repeat $ Context.typeOfTexp cond')
        caseTerms' <- zipWithM (checkType ctx) caseTerms (repeat ty)
        pure $ Typed.Match cond' (zip caseConds' caseTerms') ty

checkType ctx (AST.Let (AST.PatConstraint pat desc) expr body) ty = 
    let patTy = Context.typeOfDesc desc
    in do 
        pat' <- checkPatternType ctx pat patTy
        expr' <- checkType ctx expr patTy
        body' <- checkType ctx body ty
        pure $ Typed.Let pat' expr' body' ty

checkType ctx (AST.Let pat expr body) ty = do
    expr' <- inferType ctx expr
    pat' <- checkPatternType ctx pat (Context.typeOfTexp expr') 
    body' <- checkType ctx body ty
    pure $ Typed.Let pat' expr' body' ty

checkType ctx expr ty = do
    expr' <- inferType ctx expr
    let ty' = Context.typeOfTexp expr'
    if ty' == ty 
        then pure expr' 
        else Writer.tell ["type error: " ++ show ty' ++ " not equal to " ++ show ty ++ "\n"] $> expr'

inferType ctx (AST.Var x) =
    case Context.find x ctx of
        Just ty -> pure $ Typed.Var x ty
        Nothing -> do
            Writer.tell ["variable " ++ show x ++ " not found"]
            pure $ Typed.Error Builtin.errorType

inferType ctx (AST.Apply m1 m2) = do
    m1' <- inferType ctx m1
    case Context.typeOfTexp m1' of
        (Typed.TypeArrow t1 t2) -> do
            m2'<- checkType ctx m2 t2
            pure $ Typed.Apply m1' m2' t2
        _ -> Writer.tell ["applied term is not a function"]
             $> Typed.Error Builtin.errorType

inferType ctx (AST.Constraint m desc) =
    let t = Context.typeOfDesc desc
    in checkType ctx m t

inferType ctx (AST.Record labels exprs) =
    let ty = Context.findByLabels labels ctx
    in case ty of
        Just rcdTy@(Typed.TypeRecord _ tys) -> do
            exprs' <- zipWithM (checkType ctx) exprs tys
            pure $ Typed.Record labels exprs' rcdTy
        _ -> Writer.tell ["record type not found"]
             $> Typed.Error Builtin.errorType

inferType ctx (AST.Prim op args) = 
    let getParamsAndRetTy ty tys = 
            case ty of
                Typed.TypeArrow l r -> getParamsAndRetTy r (l:tys)
                r -> (tys,r) 
        primOpTy = Builtin.typeOfPrimOp op
        (paramTys,returnTy) = getParamsAndRetTy primOpTy []
    in do
        args' <- zipWithM (checkType ctx) args paramTys
        return $ Typed.Prim op args' returnTy

inferType _ (AST.Constant c) = 
    pure $ uncurry Typed.Constant $ case c of
        AST.Integer x -> (Typed.Integer x, Builtin.intType) 
        AST.Float x -> (Typed.Float x, Builtin.floatType)
        AST.String x -> (Typed.String x, Builtin.stringType)
        AST.Char x -> (Typed.Char x, Builtin.charType)
        AST.Unit -> (Typed.Unit, Builtin.unitType) 

inferType _ expr = error $ "unexpected " ++ show expr

checkPatternType :: Context.Context -> AST.Pattern -> Typed.Type -> W Typed.Pattern
inferPatternType :: Context.Context -> AST.Pattern -> W Typed.Pattern

checkPatternType ctx (AST.PatVar a) ty = 
    pure $ Typed.PatVar a ty

checkPatternType ctx (AST.PatTuple ps) ty@(Typed.TypeTuple tys) = do
    ps' <- zipWithM (checkPatternType ctx) ps tys
    pure $ Typed.PatTuple ps' ty

checkPatternType ctx pat ty = 
    inferPatternType ctx pat 

inferPatternType ctx (AST.PatRecord labels pats closed) =
    case Context.findByLabels labels ctx of
        Just ty@(Typed.TypeRecord _ tys) -> do 
            pats' <- zipWithM (checkPatternType ctx) pats tys
            pure $ Typed.PatRecord labels pats' closed ty
        _ -> Writer.tell ["record not found"] $> Typed.PatError Builtin.errorType

inferPatternType ctx (AST.PatConstant c) =
    pure $ uncurry Typed.PatConstant $ case c of
        AST.Integer x -> (Typed.Integer x, Builtin.intType) 
        AST.Float x -> (Typed.Float x, Builtin.floatType)
        AST.String x -> (Typed.String x, Builtin.stringType)
        AST.Char x -> (Typed.Char x, Builtin.charType)
        AST.Unit -> (Typed.Unit, Builtin.unitType) 

inferPatternType ctx pat = error $ "unexpected " ++ show pat

checkToplevelType :: Context.Context -> AST.Toplevel -> Either Typed.Toplevel [String]
checkToplevelType ctx (AST.Toplevel _ bindings) =
    let (tops, errs) = Writer.runWriter $ mapM checkBindingType bindings
    in if null errs then Left $ Typed.Toplevel tops else Right $ errs
    where
        checkBindingType AST.Binding{ AST.bindingName=name, AST.bindingExpr=expr, AST.bindingType=ty } =
            do
                let ty' = Context.typeOfDesc ty
                expr' <- checkType ctx expr ty'
                return $ Typed.Binding name expr' ty'

-- inferPatternType ctx (AST.Pat)

