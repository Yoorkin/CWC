module Typing.TypedTree(
    Type(..),
    Pattern(..),
    FieldPattern(..),
    Texp(..),
    Toplevel(..),
    Binding(..),
    Constant(..)
) where 

import qualified Parsing.AST as AST



data Type
    = TypeVar String
    | TypeTuple [Type]
    | TypeRecord [String] [Type]
    | TypeApply Type [Type]
    | TypeAbstraction [String] Type
    | TypeArrow Type Type
    | TypeTaggedUnion [(String,Type)]
    | TypeHole
    deriving(Show,Eq)

type Closed = Bool

data Pattern
    = PatVar String Type
    | PatTuple [Pattern] Type
    | PatRecord [String] [Pattern] Closed Type
    | PatConstruct [Pattern] Type
    | PatConstraint Pattern AST.TypeDesc Type
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