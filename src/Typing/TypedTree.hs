module Typing.TypedTree
    ( Type(..)
    , Pattern(..)
    , Texp(..)
    , Constant(..)
    , Program(..)
    ) where 

import qualified Parsing.AST as AST

type SchemeVar = String
type Scheme = String

data Type
    = TypeVar String
    | TypeTuple [Type]
    | TypeArrow Type Type
    -- forall-elim
    -- Represent type construction like `Map (K,V)` or `Set K`.
    -- Do not use to denote the type of value construction, e.g `Tree (Leaf 1,Leaf 2)`.
    | TypeConstruction Scheme Type
    -- forall-intro
    -- id : forall a . a -> a
    | TypeScheme [SchemeVar] Type
    | TypeHole
    deriving(Show,Eq)

data Pattern
    = PatVar String Type
    | PatTuple [Pattern] Type
    | PatConstr String Pattern Type
    | PatConstant Constant Type
    | PatHole Type
    | PatError Type
    deriving(Show,Eq)

data Texp
    = Var String Type
    | Abs Pattern Texp Type
    | Apply Texp Texp Type
    | Let Pattern Texp Texp Type
    | Letrec [String] [Texp] Texp Type
    | If Texp Texp Texp Type
    | Match Texp [Pattern] [Texp] Type
    | Tuple [Texp] Type
    | Prim AST.Operation [Texp] Type
    | Constr String Texp Type
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

data Program 
    = Program [AST.DataType] Texp 
    deriving(Show,Eq)
