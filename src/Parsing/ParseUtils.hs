module Parsing.ParseUtils(parseError,selectPrimOp) where

import Parsing.Lexer
import CPS
import Parsing.AST

parseError :: [Token] -> a
parseError tokens = error $ "Parse error: " ++ show tokens  


selectPrimOp :: Token -> Operation
selectPrimOp (TokenOp x) = 
    case x of
        "+" -> OpAdd
        "-" -> OpSub
        "*" -> OpMul
        "/" -> OpDiv
        ">=" -> OpGT
        "<=" -> OpLT
        "=" -> OpEQ
        "<>" -> OpNE
        _ -> error ""
selectPrimOp _ = error ""