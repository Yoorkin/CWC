{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}
module Typing.HMTyping where
import qualified Parsing.AST as AST
import qualified Typing.TypedTree as Typed
import qualified Typing.Builtin as Builtin
import qualified Typing.TypeContext as Ctx
import Control.Monad.State.Lazy


type Constraint = (Typed.Type, Typed.Type) 

-- id : forall a . a -> a
data Scheme 
    = Scheme [String] Typed.Type
    deriving(Show)

data S = S {
    usedNameCount::Int,
    constraints::[Constraint],
    typeSchemes::[Scheme]
}

freshName :: State S String
freshName = do
        idx <- usedNameCount <$> get
        modify (\x -> x { usedNameCount = idx + 1 })
        return $ names !! idx
    where
        names = [ a ++ show b | a <- ['a'..'z'], b <- [0..] ]

freshTypeVar :: State S Typed.Type
freshTypeVar = Typed.TypeVar <$> freshName

push :: [Typed.Type] -> State S ()
push cs = 
    let pairs = zip (init cs) (tail cs)
    in modify (\x -> x { constraints = pairs ++ constraints x })

constraintPattern :: AST.Pattern -> State s Typed.Pattern
constraintPattern _ = _

constraint :: AST.Mexp -> State S Typed.Texp
constraint (AST.Var s) = Typed.Var s <$> freshTypeVar
constraint (AST.Abs pat body) = do 
    pat' <- constraintPattern pat
    body' <- constraint body
    let ty = Typed.TypeArrow (Ctx.typeOfTpat pat') (Ctx.typeOfTexp body')
    return $ Typed.Abs pat' body' ty
constraint (AST.Apply func arg) = do
    func' <- constraint func
    arg' <- constraint arg
    x <- freshTypeVar
    let funcTy = Typed.TypeArrow (Ctx.typeOfTexp arg') x
    push [Ctx.typeOfTexp func', funcTy]
    return $ Typed.Apply func' arg' x
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
    expr' <- constraint expr
    body' <- constraint body
    push [Ctx.typeOfTpat pat', Ctx.typeOfTexp expr']
    let ty = Ctx.typeOfTexp body'
    return $ Typed.Let pat' expr' body' ty 
constraint (AST.If cond ifso ifnot) = do
    cond' <- constraint cond
    ifso' <- constraint ifso
    ifnot' <- constraint ifnot
    push [Builtin.boolType, Ctx.typeOfTexp cond']
    push [Ctx.typeOfTexp ifso',Ctx.typeOfTexp ifnot']
    return ifso'
constraint (AST.Match cond pats ms) = do
    cond' <- constraint cond
    pats' <- mapM constraintPattern pats
    ms' <- mapM constraint ms
    push (Ctx.typeOfTexp cond' : map Ctx.typeOfTpat pats')
    push (map Ctx.typeOfTexp ms')
    let ty = Ctx.typeOfTexp $ head ms'
    return $ Typed.Match cond' pats' ms' ty
constraint (AST.Tuple elems) = do
    elems' <- mapM constraint ms
    let ty = Typed.TypeTuple $ map Ctx.typeOfTexp elems'
    return $ Typed.Tuple elems' ty
constraint (AST.Record labels fields) = do
    fields' <- mapM constraint fields
    let ty = Typed.TypeRecord labels (map Ctx.typeOfTexp fields')
    return $ Typed.Record labels fields' ty
constraint (AST.Prim op args) = do
    args' <- mapM constraint args
    retTy <- freshTypeVar
    let primTy = Builtin.typeOfPrimOp op
    push [primTy, Ctx.composeArrowType (map Ctx.typeOfTexp args') retTy]
    return $ Typed.Prim op args' retTy
constraint (AST.Constraint m desc) = do
    m' <- constraint m
    let ty = Ctx.typeOfDesc desc
    push [Ctx.typeOfTexp m', ty]
    return m'
constraint (AST.Constant c) 
    = return $ uncurry Typed.Constant $ case c of
        AST.Integer x -> (Typed.Integer x, Builtin.intType) 
        AST.Float x -> (Typed.Float x, Builtin.floatType)
        AST.String x -> (Typed.String x, Builtin.stringType)
        AST.Char x -> (Typed.Char x, Builtin.charType)
        AST.Unit -> (Typed.Unit, Builtin.unitType) 
constraint AST.Hole = freshTypeVar
constraint expr = error $ "unexpected " ++ show expr  

-- type Error = Error

unify :: [Constraint] -> [(String, String)]
unify [] = []
unify (c:cs) = case c of 
    (Typed.TypeVar a, Typed.TypeVar b) 
                | a == b -> unify cs
                | a /= b -> (a, b) : unify (replace a b cs)
    (Typed.TypeRecord ls1 es1, Typed.TypeRecord ls2 es2) -> unify (zip es1 es2 : cs)
    (Typed.TypeTuple es1, Typed.TypeTuple es2) -> unify (zip es1 es2 @ cs)
    (Typed.TypeArrow a1 a2, Typed.TypeArrow b1 b2) -> unify ((a1,b1):(a2,b2):cs)
    (Typed.TypeHole, _) -> unify cs
    (_, Typed.TypeHole) -> unify cs
    _ -> error $ "cannot unify " ++ show c

replace :: String -> String -> [Constraint] -> [Constraint]
replace _ _ [] = []
replace a b ((Typed.TypeVar c,d):cs) | c == a = (b,d) : replace cs
replace a b ((c,Typed.TypeVar d):cs) | d == a = (c,b) : replace cs
replace a b ((c,d):cs) = replace cs


        


