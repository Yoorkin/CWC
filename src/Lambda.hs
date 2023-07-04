module Lambda where
import CPS

data Conrep
    = Undecided
    | Tagged Int
    | Constant Int
    | Transparent
    | TransU
    | TransB
    | Ref
    | Variable String AccessPath
    | VariableC String AccessPath

data Con
    = DataCon Conrep
    | IntCon Int
    | RealCon String
    | StringCon String

data Lexp
    = Var String
    | Fn String Lexp
    | Fix [String] [Lexp] Lexp
    | App Lexp Lexp
    | Integer Int
    | String_ String
    | Switch Lexp [Conrep] [(Con, Lexp)] (Maybe Lexp)
    | Con Conrep Lexp
    | Decon Conrep Lexp
    | Record [Lexp]
    | Select Int Lexp
    | Raise Lexp
    | Handle Lexp Lexp
    | Prim PrimOp
    

