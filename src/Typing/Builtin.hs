module Typing.Builtin where

import qualified Typing.TypedTree as Typed
import qualified Parsing.AST as AST

intType, boolType, floatType, stringType, charType, unitType, intAddType, intSubType, intMulType,intDivType,intNegType  :: Typed.Type
intType = Typed.TypeVar "int"
boolType = Typed.TypeVar "bool" 
floatType = Typed.TypeVar "float"
stringType = Typed.TypeVar "string"
charType = Typed.TypeVar "char"
unitType = Typed.TypeVar "unit"
errorType = Typed.TypeVar "<error>"

(|->) = Typed.TypeArrow
infixr |->

intAddType = intType |-> intType |-> intType
intSubType = intAddType
intMulType = intAddType
intDivType = intAddType

intNegType = intType |-> intType

typeOfPrimOp :: AST.Operation -> Typed.Type
typeOfPrimOp op = 
    case op of
        AST.OpLT -> intType |-> intType |-> boolType
        AST.OpAdd -> intAddType
        AST.OpSub -> intSubType
        _ -> error $ "unexpected " ++ show op 

