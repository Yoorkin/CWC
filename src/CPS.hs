module CPS(
    PrimOp(..), 
    Value(..), 
    AccessPath(..), 
    Cexp(..)) where

data PrimOp 
    = Mul | Add | Sub | Div | Not
    | Ieql | Ineq | Lt | Le | Gt | Ge 
    | RangeChk | Subscript | Ordof | Assign
    | UnboxedAssign | Update | UnboxedUpdate
    | Store | Makeref | MakerefUnboxed | Alength | Slength
    | Gethdlr | Sethdlr | Boxed
    | Fadd | Fsub | Fdiv | Fmul
    | Feql | Fneq | Fge | Fgt | Fle | Flt
    | RShift | LShift | Orb | Andb | Xorb | Notb
    deriving(Show,Eq)

data Value
    = Var String
    | Label String
    | Int_ Int
    | Real Float
    | String_ String
    deriving(Show,Eq)

data AccessPath 
    = OFFp Int 
    | SELp (Int,AccessPath)
    deriving(Show,Eq)

data Cexp
    = Record [(Value, AccessPath)] String Cexp
    | Select Int Value String Cexp
    | Offset Int Value String Cexp
    | App Value [Value]
    | Fix [(String, [String], Cexp)] Cexp
    | Switch Value [Cexp]
    | Prim PrimOp [Value] [String] [Cexp]
    deriving(Show,Eq)
