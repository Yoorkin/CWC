module Typing.TypedTree(
    Type(..),
    Pattern(..),
    FieldPattern(..),
    Texp(..),
    Toplevel(..),
    Binding(..),
    Constant(..),
    Scheme(..)
) where 

import qualified Parsing.AST as AST

-- id : forall a . a -> a
data Scheme 
    = Scheme [String] Type
    deriving(Show)

data Type
    = TypeVar String
    | TypeTuple [Type]
    | TypeArrow Type Type
    | TypeConstr String [Type]
    | TypeHole
    deriving(Show,Eq)

type Closed = Bool

data Pattern
    = PatVar String Type
    | PatTuple [Pattern] Type
    | PatConstr [Pattern] Type
    | PatConstant Constant Type
    | PatHole Type
    | PatError Type
    deriving(Show,Eq)

data FieldPattern 
    = PatField String Pattern
    | PatRest 
    deriving(Show,Eq)

data Texp
    = Var String Type
    | Abs Pattern Texp Type
    | Apply Texp Texp Type
    | Let Pattern Texp Texp Type
    | Letrec [String] [Texp] Texp Type
    | If Texp Texp Texp Type
    | Match Texp [Pattern] [Texp] Type
    | Prim AST.Operation [Texp] Type
    | Tuple [Texp] Type
    | Record [String] [Texp] Type
    | Constant Constant Type
    | Hole Type
    | Error Type
    deriving(Show,Eq)

data Constant 
    = Integer Int
    | Boolean Bool
    | Float Float
    | String String
    | Char Char
    | Unit
    deriving(Show,Eq)

data Toplevel
    = Toplevel [Binding] deriving(Show,Eq)

data Binding = Binding String Texp Type deriving(Show,Eq)