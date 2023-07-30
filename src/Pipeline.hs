module Pipeline where

import qualified Parsing.Lexer as Lexer
import qualified Parsing.AST as AST
import qualified Parsing.YmlParser as Parser
import qualified Typing.TypeContext as Context

-- compile source = 
--     case Lexer.tokenize source of
--         Left x -> error (show x)
--         Right tokens -> 
--             let 
--                 toplevels = Parser.parse tokens
--             in result

