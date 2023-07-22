module Parsing.AST(
    GenericArg, 
    Toplevel(..), 
    Pattern(..), 
    TypeDesc(..),
    Mexp(..),
    Operation(..),
    Constant(..),
    TypeData(..),
    Binding(..)) where

data TypeDesc
    = TypeDescVar String
    | TypeDescApply TypeDesc TypeDesc
    | TypeDescTuple [TypeDesc]
    | TypeDescRecord [String] [TypeDesc]
    | TypeDescArrow TypeDesc TypeDesc
    | TypeDescTaggedUnion [(String,TypeDesc)]
    deriving(Show,Eq)

type GenericArg = String

data Toplevel = Toplevel [TypeData] [Binding] deriving(Show,Eq)

data TypeData = TypeData { 
    typeDataName :: String,
    typeDataQuantifier :: [GenericArg],
    typeDataDesc :: TypeDesc
 } deriving(Show,Eq)

data Binding = Binding {
    bindingName :: String,
    bindingType ::  TypeDesc,
    bindingExpr :: Mexp
 } deriving(Show,Eq)

type Closed = Bool

data Pattern
    = PatVar String
    | PatHole
    | PatTuple [Pattern]
    | PatRecord [String] [Pattern] Closed
    | PatConstraint Pattern TypeDesc
    | PatConstant Constant
    | PatConstruct [Pattern]
    deriving(Show,Eq)

data Mexp
    = Var String
    | Abs Pattern Mexp
    | Apply Mexp Mexp
    | Let Pattern Mexp Mexp
    | Letrec [String] [Mexp] Mexp
    | If Mexp Mexp Mexp
    | Match Mexp [(Pattern, Mexp)]
    | Prim Operation [Mexp]
    | Tuple [Mexp]
    | Record [String] [Mexp]
    | Constraint Mexp TypeDesc
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