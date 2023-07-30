module Parsing.AST
    ( Program(..)
    , DataType(..) 
    , Constructor(..)
    , Pattern(..) 
    , Annotation(..)
    , Mexp(..)
    , Operation(..)
    , Constant(..)
    ) where

data Program 
    = Program [DataType] Mexp 
    deriving(Show,Eq)

type SchemeVar = String

data DataType 
    = DataType String [SchemeVar] [Constructor] 
    deriving(Show,Eq)

data Constructor
    = Constructor String Annotation
    deriving(Show,Eq)

data Annotation
    = AnnoVar String
    | AnnoTuple [Annotation]
    | AnnoArrow Annotation Annotation
    -- use `AnnoConstr "Map" (AnnoTuple [AnnoVar "K", AnnoVar "V"])` to represent type construction `Map(K,V)`
    | AnnoTypeConstr String Annotation
    deriving(Show,Eq)

data Pattern
    = PatVar String
    | PatTuple [Pattern]
    -- use `PatConstr "Tree" (PatTuple [PatVar "a", PatVar "b"])` to represent pattern `Tree(a,b)`
    | PatConstr String Pattern
    | PatConstant Constant
    | PatConstraint Pattern Annotation
    | PatHole
    deriving(Show,Eq)

data Mexp
    = Var String
    | Abs Pattern Mexp
    -- `Tree (Leaf 5, Leaf 6)` or `zip (ls1,ls2)` or `id 5`
    -- Represent data construct and function application 
    -- due to Yml cannot distinguish them by syntax
    | Apply Mexp Mexp
    | Let Pattern Mexp Mexp
    | Letrec [String] [Mexp] Mexp
    | If Mexp Mexp Mexp
    | Match Mexp [Pattern] [Mexp]
    -- (1,2,'a')
    | Tuple [Mexp]
    -- 1 + 2
    | Prim Operation [Mexp]
    -- t : T
    | Constraint Mexp Annotation
    -- 114514
    | Constant Constant
    -- _
    | Hole
    deriving(Show,Eq)

data Constant 
    = Integer Int
    | Boolean Bool
    | Float Float
    | String String
    | Char Char
    | Unit
    deriving(Show,Eq)

data Operation
    = OpAdd | OpSub | OpMul | OpDiv
    | OpGT | OpGE | OpNE | OpEQ | OpLE | OpLT
    | OpNot | OpAnd | OpOr | OpXor
    deriving(Show,Eq)

-- instance Show Mexp where
--     show (Var str) = str
--     show (Abs pat expr) = "fun " ++ show pat ++ " -> " ++ show expr
--     show (Apply m1 m2) = show m1 ++ "(" ++ show m2 ++ ")"
--     show (Let pat expr body) = "let " ++ show pat ++ " = " ++ show expr ++ " in " ++ show expr
--     show (Letrec)

-- instance Show Constant where
--     show (Integer x) = show x
--     show (Boolean x) = show x
--     show (Float x) = show x
--     show (String x) = show x
--     show (Char x) = show x
--     show Unit = "()"

-- instance Show Operation where
--     show OpAdd = "+"
--     show OpSub = "-"
--     show OpMul = "*"
--     show OpDiv = "/"
--     show OpGT = ">"
--     show OpGE = ">="
--     show OpNE = "<>"
--     show OpEQ = "=="
--     show OpLE = "<="
--     show OpLT = "<"
--     show OpNot = "not"
--     show OpAnd = "and"
--     show OpOr = "or"
--     show OpXor = "xor"