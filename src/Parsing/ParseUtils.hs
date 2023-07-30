module Parsing.ParseUtils(
        parseError,
        selectPrimOp) where

import Parsing.Lexer
import CPS
import Parsing.AST
import Data.List (sortBy)
import Data.Function (on)

parseError :: [Token] -> a
parseError tokens = error $ "Parse error: " ++ show tokens  


selectPrimOp :: Token -> Operation
selectPrimOp (TokenOp x) = 
    case x of
        "+" -> OpAdd
        "-" -> OpSub
        "*" -> OpMul
        "/" -> OpDiv
        ">" -> OpGT
        "<" -> OpLT
        ">=" -> OpGE
        "<=" -> OpLE
        "=" -> OpEQ
        "<>" -> OpNE
        _ -> error $ show x
selectPrimOp _ = error ""




