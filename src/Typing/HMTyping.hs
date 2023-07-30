{-# LANGUAGE TemplateHaskell #-}

module Typing.HMTyping where
import qualified Parsing.AST as AST
import qualified Typing.TypedTree as Typ
import qualified Typing.Builtin as Builtin
import qualified Typing.TypeContext as Ctx
import qualified Data.Map as Map
import Control.Monad.State.Lazy as State
import Control.Lens
import Data.Maybe

type Constraint = (Typ.Type, Typ.Type) 

data UnifyCtx = UnifyCtx {
    _patternFreeVars :: [(String, Typ.Type)],
    _bindings :: Map.Map String [Typ.Type],
    _usedNameCount :: Int,
    _constraints :: [Constraint],
    _functions :: Map.Map String Typ.Scheme
} deriving(Show)

makeLenses ''UnifyCtx

data Result = Result { 
    resultExpr :: Typ.Texp, 
    resultCtx :: UnifyCtx, 
    resultConstr :: [Constraint] 
} deriving(Show)

typing :: AST.Program -> Result
typing (AST.Program datas expr) = 
    let initialCtx = UnifyCtx [] Map.empty 0 [] (Map.fromList $ scanDataTypes datas) 
        (exp,ctx) = runState (constraint expr) initialCtx
    in Result exp ctx (unify (ctx^.constraints) [])


scanDataTypes :: [AST.DataType] -> [(String, Typ.Scheme)]
scanDataTypes = concatMap dataType2tys 
        where 
            dataType2tys (AST.DataType typeName tyVars ctors) = 
                let retTy = if null tyVars 
                            then Typ.TypeVar typeName 
                            else Typ.TypeConstr typeName (map Typ.TypeVar tyVars)
                    ctors2tys (AST.Constructor name anno) = 
                        (name, Typ.Scheme tyVars $ Typ.TypeArrow (Ctx.typeOfAnno anno) retTy) 
                in map ctors2tys ctors

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

pushBindings :: [(String,Typ.Type)] -> State.State UnifyCtx ()
pushBindings binds = do
        bs <- use bindings
        bindings .= push binds bs
    where push [] m = m
          push ((name,ty):xs) m = 
            Map.alter (Just . (:) ty . fromMaybe []) name (push xs m)

popBindings :: [String] -> State.State UnifyCtx ()
popBindings names = do
        bs <- use bindings
        bindings .= pop names bs 
    where pop [] m = m
          pop (name:xs) m = 
            Map.alter (fmap tail) name (pop xs m)


getBinding :: String -> State.State UnifyCtx (Maybe Typ.Type)
getBinding name = fmap head <$> Map.lookup name <$> use bindings

constraintPattern :: AST.Pattern -> State.State UnifyCtx Typ.Pattern
constraintPattern (AST.PatVar name) = do 
        ty <- freshTypeVar
        fv <- use patternFreeVars
        patternFreeVars .= ((name, ty) : fv) 
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
constraintPattern (AST.PatConstant const) = 
    return $ uncurry Typ.PatConstant $ case const of
        AST.Integer x -> (Typ.Integer x, Builtin.intType)
        AST.Boolean x -> (Typ.Boolean x, Builtin.boolType)
        AST.Float x -> (Typ.Float x, Builtin.floatType)
        AST.String x -> (Typ.String x, Builtin.stringType)
        AST.Char x -> (Typ.Char x, Builtin.charType)
        AST.Unit -> (Typ.Unit, Builtin.unitType)


constraint :: AST.Mexp -> State UnifyCtx Typ.Texp
constraint (AST.Var name) = do
    maybeTy <- getBinding name
    case maybeTy of
        Just ty -> return $ Typ.Var name ty
        Nothing -> Typ.Var name <$> freshTypeVar
constraint (AST.Abs pat body) = do 
    pat' <- constraintPattern pat
    fv <- use patternFreeVars
    patternFreeVars .= []
    pushBindings fv
    body' <- constraint body
    popBindings (fmap fst fv)
    let ty = Typ.TypeArrow (Ctx.typeOfTpat pat') (Ctx.typeOfTexp body')
    return $ Typ.Abs pat' body' ty
constraint (AST.Apply func arg) = do
    func' <- constraint func
    arg' <- constraint arg
    x <- freshTypeVar
    let funcTy = Typ.TypeArrow (Ctx.typeOfTexp arg') x
    push [Ctx.typeOfTexp func', funcTy]
    return $ Typ.Apply func' arg' x
-- constraint (AST.Let pat expr@(AST.Abs _ _) body) = 
--     initState i = State {usedNameCount = i, constraints = []} 
--     calcState = (,) <$> constraintPattern pat <*> constraint expr
--     typingBinding usedName = 
--         let (state,binding) = runState calcState initState
--         in case unify (constraints state) of
--             Left 
                
--     in 

--     body' <- constraint body
--     push 
constraint (AST.Let pat expr body) = do
    pat' <- constraintPattern pat
    fv <- use patternFreeVars
    expr' <- constraint expr
    case expr' of
        Typ.Abs _ -> do 
                fns <- use functions
                 
        _ -> do patternFreeVars .= []
                pushBindings fv
    body' <- constraint body
    push [Ctx.typeOfTpat pat', Ctx.typeOfTexp expr']
    popBindings (map fst fv)
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
            let p = replace a b in unify (p cs) ((a,b) : p acc)
    (_, Typ.TypeVar y) | y `notElem` free a -> 
            let p = replace b a in unify (p cs) ((b,a) : p acc)
    (Typ.TypeTuple es1, Typ.TypeTuple es2) -> 
            unify (zip es1 es2 ++ cs) acc
    (Typ.TypeArrow a1 a2, Typ.TypeArrow b1 b2) -> 
            unify ((a1,b1):(a2,b2):cs) acc
    (Typ.TypeHole, _) -> unify cs acc
    (_, Typ.TypeHole) -> unify cs acc
    _ -> error $ "cannot unify " ++ show c

replace :: Typ.Type -> Typ.Type -> [Constraint] -> [Constraint]
replace a b cs = fmap (\(x,y) -> (replace' x, replace' y)) cs
        where 
            replace' ty = 
                case ty of
                    (Typ.TypeVar _) -> if ty == a then b else ty
                    (Typ.TypeTuple xs) -> Typ.TypeTuple (fmap replace' xs)
                    (Typ.TypeArrow l r) -> Typ.TypeArrow (replace' l) (replace' r)
                    (Typ.TypeConstr n xs) -> Typ.TypeConstr n (fmap replace' xs)
                    Typ.TypeHole -> Typ.TypeHole 


free :: Typ.Type -> [String]
free (Typ.TypeVar var@('\'' : a)) = [var]
free (Typ.TypeVar _) = []
free (Typ.TypeTuple xs) = concatMap free xs
free (Typ.TypeArrow x y) = free x ++ free y
free (Typ.TypeConstr _ xs) = concatMap free xs
free Typ.TypeHole = []  

        


