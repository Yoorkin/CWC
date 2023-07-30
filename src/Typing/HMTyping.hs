{-# LANGUAGE TemplateHaskell #-}

module Typing.HMTyping where
import qualified Parsing.AST as AST
import qualified Typing.TypedTree as Typ
import qualified Typing.Builtin as Builtin
import qualified Typing.TypeContext as Ctx
import qualified Data.Map as Map
import Text.Pretty.Simple
import qualified Data.Text.Lazy as Text
import Control.Monad.State.Lazy as State
import Control.Lens
import Data.Maybe

type Constraint = (Typ.Type, Typ.Type)

data BindingKind = LetBinding | DataConstructor deriving(Show,Eq)

data Binding = Binding
    { _bindingName :: String
    , _bindingType :: Typ.Type
    , _bindingKind :: BindingKind
    } deriving(Show,Eq)

makeLenses ''Binding

data UnifyCtx =  UnifyCtx
    { _usedNameCount :: Int
    , _constraints :: [Constraint]
    , _patternVars :: [Binding]
    , _bindings :: Map.Map String [Binding]
    } deriving(Show)

makeLenses ''UnifyCtx

data Result = Result
    { resultExpr :: Typ.Texp
    , resultCtx :: UnifyCtx
    , resultSolution :: [Constraint]
    , resultRewrited :: Typ.Texp
    } deriving(Show)

typing :: AST.Program -> Result
typing (AST.Program datas expr) =
    let initialCtx = UnifyCtx
            { _usedNameCount = 0
            , _constraints = []
            , _patternVars = []
            , _bindings = Map.fromList $ scanDataTypes datas
            }
        (exp,ctx) = runState (constraint expr) initialCtx
        solution = unify (ctx^.constraints) []
        rewritedExp = rewriteBySolution solution exp
    in Result exp ctx solution rewritedExp

-- typingWithInitialContext :: UnifyCtx -> AST.Mexp -> Result 
-- typingWithInitialContext ctx expr = 
--     let (expr', ctx) = runState (constraint expr) ctx
--     in 

rewriteBySolution :: [Constraint] -> Typ.Texp -> Typ.Texp
rewriteBySolution [] exp = exp 
rewriteBySolution ((a, b) : solution) exp = rewriteBySolution solution (rewriteTexp exp)
    where rewriteTexp expr =
            case expr of
                (Typ.Var s ty) -> Typ.Var s (rewriteType a b ty)
                (Typ.Abs p e ty) -> Typ.Abs (rewriteTpat p) (rewriteTexp e) (rewriteTyp ty)
                (Typ.Apply e1 e2 ty) -> Typ.Apply (rewriteTexp e1) (rewriteTexp e2) (rewriteTyp ty)
                (Typ.Let p e1 e2 ty) -> Typ.Let (rewriteTpat p) (rewriteTexp e1) (rewriteTexp e2) (rewriteTyp ty)
                (Typ.If e1 e2 e3 ty) -> Typ.If (rewriteTexp e1) (rewriteTexp e2) (rewriteTexp e3) (rewriteTyp ty)
                (Typ.Match e pats exprs ty) -> Typ.Match (rewriteTexp e) (map rewriteTpat pats) (map rewriteTexp exprs) (rewriteTyp ty)
                (Typ.Tuple exprs ty) -> Typ.Tuple (map rewriteTexp exprs) (rewriteTyp ty)
                (Typ.Prim op exprs ty) -> Typ.Prim op (map rewriteTexp exprs) (rewriteTyp ty)
                (Typ.Constr s e ty) -> Typ.Constr s (rewriteTexp e) (rewriteTyp ty)
                (Typ.Constant c ty) -> Typ.Constant c (rewriteTyp ty)
                (Typ.Hole ty) -> Typ.Hole (rewriteTyp ty)
                (Typ.Error ty) -> Typ.Error (rewriteTyp ty)
                _ -> error "" 
          rewriteTpat pat =
            case pat of
                (Typ.PatVar s ty) -> Typ.PatVar s (rewriteTyp ty)
                (Typ.PatTuple pats ty) -> Typ.PatTuple (map rewriteTpat pats) (rewriteTyp ty)
                (Typ.PatConstr s pat ty) -> Typ.PatConstr s (rewriteTpat pat) (rewriteTyp ty)
                (Typ.PatConstant c ty) -> Typ.PatConstant c (rewriteTyp ty)
                (Typ.PatHole ty) -> Typ.PatHole (rewriteTyp ty)
                (Typ.PatError ty) -> Typ.PatError (rewriteTyp ty)
          rewriteTyp = rewriteType a b


scanDataTypes :: [AST.DataType] -> [(String, [Binding])]
scanDataTypes = concatMap dataType2tys
        where
            dataType2tys (AST.DataType dataTyName schemeVars ctors) =
                let retTy =
                        case schemeVars of
                            [] -> Typ.TypeVar dataTyName
                            [x] -> Typ.TypeConstruction dataTyName (Typ.TypeVar x)
                            xs -> Typ.TypeConstruction dataTyName (Typ.TypeTuple (map Typ.TypeVar xs))
                    ctor2ty anno =
                        Typ.TypeScheme schemeVars $ Typ.TypeArrow (Ctx.typeOfAnno anno) retTy
                    ctor2binding (AST.Constructor name anno) =
                        (name, [Binding name (ctor2ty anno) DataConstructor])
                in map ctor2binding ctors

freshName :: State.State UnifyCtx String
freshName = do
        let names = ['\'' : a : if b==0 then "" else show b | b <- [0..], a <- ['a'..'z']]
        idx <- use usedNameCount
        usedNameCount += 1
        return (names !! idx)

freshTypeVar :: State.State UnifyCtx Typ.Type
freshTypeVar = Typ.TypeVar <$> freshName

push :: [Typ.Type] -> State.State UnifyCtx ()
push cs = do
    let ps = zip (init cs) (tail cs)
    cs <- use constraints
    constraints .= ps ++ cs

pushBindings :: [Binding] -> State.State UnifyCtx ()
pushBindings binds = do
        bs <- use bindings
        bindings .= push binds bs
    where push [] m = m
          push (bind@(Binding name _ _):xs) m =
            Map.alter (Just . (:) bind . fromMaybe []) name (push xs m)

popBindings :: [String] -> State.State UnifyCtx ()
popBindings names = do
        bs <- use bindings
        bindings .= pop names bs
    where pop [] m = m
          pop (name:xs) m =
            Map.alter (fmap tail) name (pop xs m)


getBinding :: String -> State.State UnifyCtx (Maybe Binding)
getBinding name = fmap head . Map.lookup name <$> use bindings

constraintPattern :: AST.Pattern -> State.State UnifyCtx Typ.Pattern
constraintPattern (AST.PatVar name) = do
        ty <- freshTypeVar
        fv <- use patternVars
        patternVars .= (Binding name ty LetBinding : fv)
        return $ Typ.PatVar name ty

constraintPattern AST.PatHole = Typ.PatHole <$> freshTypeVar

constraintPattern (AST.PatTuple elems) = do
        elems' <- mapM constraintPattern elems
        let ty = Typ.TypeTuple (map Ctx.typeOfTpat elems')
        return $ Typ.PatTuple elems' ty

constraintPattern (AST.PatConstraint pat anno) = do
        pat' <- constraintPattern pat
        push [Ctx.typeOfTpat pat', Ctx.typeOfAnno anno]
        return pat'

constraintPattern (AST.PatConstr name pat) = do
    maybeBind <- getBinding name
    case maybeBind of
        Just (Binding _ ty DataConstructor) -> do
            pat' <- constraintPattern pat
            let (elemTy,dataTy) =
                    case ty of
                        Typ.TypeScheme _ (Typ.TypeArrow argTy retTy) -> (argTy, retTy)
                        Typ.TypeArrow argTy retTy -> (argTy, retTy)
                        _ -> error ""
            let patTy = Ctx.typeOfTpat pat'
            push [patTy, elemTy]
            return $ Typ.PatConstr name pat' dataTy
        _ -> error $ name ++ " is not a data constructor or not found"

constraintPattern (AST.PatConstant const) =
    return $ uncurry Typ.PatConstant $ case const of
        AST.Integer x -> (Typ.Integer x, Builtin.intType)
        AST.Boolean x -> (Typ.Boolean x, Builtin.boolType)
        AST.Float x -> (Typ.Float x, Builtin.floatType)
        AST.String x -> (Typ.String x, Builtin.stringType)
        AST.Char x -> (Typ.Char x, Builtin.charType)
        AST.Unit -> (Typ.Unit, Builtin.unitType)

applyScheme :: Typ.Type -> Typ.Type -> Typ.Type
applyScheme (Typ.TypeScheme svars schemeTy) varsTy = 
        let vars = 
                case varsTy of
                    (Typ.TypeVar v) -> [v]
                    (Typ.TypeTuple vs) -> fmap (\(Typ.TypeVar v) -> v) vs
                    _ -> error ""
        in go svars vars schemeTy
    where 
        go [] [] resultTy = resultTy
        go (svar:svars) (var:vars) schemeTy = go svars vars $ rewriteType (Typ.TypeVar svar) (Typ.TypeVar var) schemeTy
        go _ _ _ = error ""
applyScheme _ _ = error ""

constraint :: AST.Mexp -> State UnifyCtx Typ.Texp
constraint (AST.Var name) = do
    maybeBind <- getBinding name
    case maybeBind of
        Just (Binding _ ty _) -> return $ Typ.Var name ty
        Nothing -> Typ.Var name <$> freshTypeVar

constraint (AST.Abs pat body) = do
    pat' <- constraintPattern pat
    fv <- use patternVars
    patternVars .= []
    pushBindings fv
    body' <- constraint body
    popBindings (fmap (^.bindingName) fv)
    let ty = Typ.TypeArrow (Ctx.typeOfTpat pat') (Ctx.typeOfTexp body')
    return $ Typ.Abs pat' body' ty

constraint (AST.Apply func arg) = do
    func' <- constraint func
    arg' <- constraint arg
    x <- freshTypeVar
    let actualFuncTy = case Ctx.typeOfTexp func' of
            (Typ.TypeScheme _ _) -> applyScheme (Ctx.typeOfTexp func') (Ctx.typeOfTexp arg')
            _ -> Ctx.typeOfTexp func'
    let expectedFuncTy = Typ.TypeArrow (Ctx.typeOfTexp arg') x
    push [actualFuncTy, expectedFuncTy]
    return $ Typ.Apply (Ctx.updateTypeOfTexp actualFuncTy func') arg' x

constraint (AST.Let pat expr body) = do
    pat' <- constraintPattern pat
    fv <- use patternVars
    expr' <- constraint expr
    -- case expr' of
    --     Typ.Abs _ -> do
    --             fns <- use functions

    --     _ -> do 
            -- patternFreeVars .= []
            -- pushBindings fv
    patternVars .= []
    pushBindings fv
    body' <- constraint body
    push [Ctx.typeOfTpat pat', Ctx.typeOfTexp expr']
    popBindings (map (^.bindingName) fv)
    let ty = Ctx.typeOfTexp body'
    return $ Typ.Let pat' expr' body' ty

constraint (AST.If cond ifso ifnot) = do
    cond' <- constraint cond
    ifso' <- constraint ifso
    ifnot' <- constraint ifnot
    push [Ctx.typeOfTexp cond', Builtin.boolType]
    retTy <- freshTypeVar
    push [Ctx.typeOfTexp ifso', retTy]
    push [Ctx.typeOfTexp ifnot', retTy]
    return $ Typ.If cond' ifso' ifnot' retTy

constraint (AST.Match cond pats ms) = do
    cond' <- constraint cond
    pats' <- mapM constraintPattern pats
    ms' <- mapM constraint ms
    push (Ctx.typeOfTexp cond' : map Ctx.typeOfTpat pats')
    push (map Ctx.typeOfTexp ms')
    let ty = Ctx.typeOfTexp $ head ms'
    return $ Typ.Match cond' pats' ms' ty

constraint (AST.Tuple elems) = do
    elems' <- mapM constraint elems
    let ty = Typ.TypeTuple $ map Ctx.typeOfTexp elems'
    return $ Typ.Tuple elems' ty

constraint (AST.Prim op args) = do
    args' <- mapM constraint args
    retTy <- freshTypeVar
    let primTy = Builtin.typeOfPrimOp op
    push [primTy, Ctx.composeArrowType (map Ctx.typeOfTexp args') retTy]
    return $ Typ.Prim op args' retTy

constraint (AST.Constraint term anno) = do
    term' <- constraint term
    let ty = Ctx.typeOfAnno anno
    push [Ctx.typeOfTexp term', ty]
    return term'

constraint (AST.Constant c)
    = return $ uncurry Typ.Constant $ case c of
        AST.Integer x -> (Typ.Integer x, Builtin.intType)
        AST.Float x -> (Typ.Float x, Builtin.floatType)
        AST.String x -> (Typ.String x, Builtin.stringType)
        AST.Char x -> (Typ.Char x, Builtin.charType)
        AST.Unit -> (Typ.Unit, Builtin.unitType)
        AST.Boolean x -> (Typ.Boolean x, Builtin.boolType)
        _ -> error "unexpected constant"

constraint AST.Hole = Typ.Var "_" <$> freshTypeVar
constraint expr = error $ "unexpected " ++ show expr

-- type Error = Error

unify :: [Constraint] -> [Constraint] -> [Constraint]
unify [] acc = acc
unify (c@(a,b):cs) acc = case c of
    (Typ.TypeVar x, Typ.TypeVar y)
            | x == y -> unify cs acc
            | head x /= '\'' && head y == '\'' -> unify ((b,a):cs) acc
    (Typ.TypeVar x, _) | x `notElem` free b ->
            let p = rewriteConstraint a b in unify (p cs) ((a,b) : p acc)
    (_, Typ.TypeVar y) | y `notElem` free a ->
            let p = rewriteConstraint b a in unify (p cs) ((b,a) : p acc)
    (Typ.TypeTuple es1, Typ.TypeTuple es2) ->
            unify (zip es1 es2 ++ cs) acc
    (Typ.TypeArrow a1 a2, Typ.TypeArrow b1 b2) ->
            unify ((a1,b1):(a2,b2):cs) acc
    (Typ.TypeConstruction scheme1 x, Typ.TypeConstruction scheme2 y) 
            | scheme1 == scheme2 -> unify ((x,y):cs) acc
    -- (Typ.TypeScheme _ _, _) -> unify cs (c:acc)
    (Typ.TypeHole, _) -> unify cs acc
    (_, Typ.TypeHole) -> unify cs acc
    _ -> error $ "cannot unify " ++ Text.unpack (pShow c)

rewriteType :: Typ.Type -> Typ.Type -> Typ.Type -> Typ.Type
rewriteType a b = go 
    where go ty = case ty of
            (Typ.TypeVar _) -> if ty == a then b else ty
            (Typ.TypeTuple xs) -> Typ.TypeTuple (fmap go xs)
            (Typ.TypeArrow l r) -> Typ.TypeArrow (go l) (go r)
            (Typ.TypeConstruction scheme ty') -> Typ.TypeConstruction scheme (go ty')
            (Typ.TypeScheme sv ty') -> Typ.TypeScheme sv (go ty')
            Typ.TypeHole -> Typ.TypeHole

rewriteConstraint :: Typ.Type -> Typ.Type -> [Constraint] -> [Constraint]
rewriteConstraint a b cs = fmap (\(x,y) -> (replace' x, replace' y)) cs
        where replace' = rewriteType a b

free :: Typ.Type -> [String]
free (Typ.TypeVar var@('\'' : a)) = [var]
free (Typ.TypeVar _) = []
free (Typ.TypeTuple xs) = concatMap free xs
free (Typ.TypeArrow x y) = free x ++ free y
free (Typ.TypeConstruction _ ty) = free ty
free (Typ.TypeScheme _ ty) = free ty
free Typ.TypeHole = []




