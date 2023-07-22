module Parsing.ParseUtils(
        parseError,
        selectPrimOp,
        pickToplevel,
        Top(..),
        fieldPatsToRecordPattern,
        processRecordFields,
        FieldPattern(..)) where

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


data FieldPattern 
    = PatField String Pattern
    | PatRest 
    deriving(Show,Eq)

fieldPatsToRecordPattern :: [FieldPattern] -> Pattern
fieldPatsToRecordPattern xs = 
        let loop xs fs closed =
                case xs of 
                        (PatField l p) : xs' -> loop xs' ((l,p) : fs) closed
                        PatRest : xs' -> loop xs' fs False
                        [] -> (sortByFst fs, closed)
            (fs,closed) = loop xs [] True
            (ls,ps) = unzip fs
         in PatRecord ls ps closed

sortByFst = sortBy (compare `on` fst)

processRecordFields :: [(String,a)] -> ([String],[a])
processRecordFields xs = unzip $ sortByFst xs


data Top
  = TopTypeData TypeData
  | TopBinding Binding

isTypeData x = case x of
        (TopTypeData _) -> True
        _ -> False

isBinding x = case x of
        (TopBinding _) -> True
        _ -> False

fromTypeData x = 
    case x of
        (TopTypeData d) -> d
        _ -> error ""

fromBinding x = 
    case x of
        (TopBinding d) -> d
        _ -> error ""

pickToplevel xs = (fromTypeData <$> filter isTypeData xs, fromBinding <$> filter isBinding xs)