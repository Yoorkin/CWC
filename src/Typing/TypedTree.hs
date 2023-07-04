module TypedTree where 

data Type
    = TypeVar String
    | TypeApply Type Type
    | TypeTuple [Type]
    | TypeRecord [(String, Type)]
    | TypeArrow Type Type
    deriving(Show,Eq)

type GenericArg = String

data Toplevel
    = ToplevelType String [GenericArg] [(String, Type)]
    | ToplevelLet Pattern Mexp
    | ToplevelLetrec [String] [Mexp]
    deriving(Show,Eq)

data Pattern
    = PatVar String
    | PatTuple [Pattern]
    | PatRecord [FieldPattern]
    | PatConstraint Pattern Type
    deriving(Show,Eq)

data FieldPattern 
    = PatField String Pattern
    | PatRest 
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
    | Record [(String, Mexp)]
    | Constraint Mexp Type
    | Integer Int
    | Boolean Bool
    | Float_ Float
    | String_ String
    | Char_ Char
    | Unit
    deriving(Show,Eq)

data Operation
    = OpAdd | OpSub | OpMul | OpDiv
    | OpGT | OpGE | OpNE | OpEQ | OpLE | OpLT
    | OpNot | OpAnd | OpOr | OpXor
    deriving(Show,Eq)

data Module = Module [Toplevel]
