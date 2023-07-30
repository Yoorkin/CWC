module Parsing.AST(
    Program(..), 
    DataType(..), 
    Constructor(..),
    Pattern(..), 
    Annotation(..),
    Mexp(..),
    Operation(..),
    Constant(..)) where

data Program 
    = Program [DataType] Mexp 
    deriving(Show,Eq)

type TypeVar = String
type TypeConstr = String

data DataType 
    = DataType String [TypeVar] [Constructor] 
    deriving(Show,Eq)

data Constructor
    = Constructor String Annotation
    deriving(Show,Eq)

data Annotation
    = AnnoVar String
    | AnnoTuple [Annotation]
    | AnnoArrow Annotation Annotation
    | AnnoConstr String [Annotation]
    deriving(Show,Eq)

data Pattern
    = PatVar String
    | PatHole
    | PatTuple [Pattern]
    -- | PatRecord [String] [Pattern] Closed
    | PatConstraint Pattern Annotation
    | PatConstant Constant
    | PatConstr [Pattern]
    deriving(Show,Eq)

data Mexp
    = Var String
    | Abs Pattern Mexp
    | Apply Mexp Mexp
    | Let Pattern Mexp Mexp
    | Letrec [String] [Mexp] Mexp
    | If Mexp Mexp Mexp
    | Match Mexp [Pattern] [Mexp]
    | Tuple [Mexp]
    | Record [String] [Mexp]
    | Prim Operation [Mexp]
    | Constraint Mexp Annotation
    | Constant Constant
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