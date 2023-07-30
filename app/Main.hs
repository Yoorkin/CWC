module Main (main) where

import qualified Parsing.Lexer as Lexer
import qualified Parsing.YmlParser as Parser
import qualified Typing.HMTyping as Typer
import System.IO
import System.Directory
import Text.Pretty.Simple

readSource path = do
    file <- openFile path ReadMode
    hGetContents file

parse source = 
    case Lexer.tokenize source of
        Left x -> error (show x)
        Right tokens -> Parser.parse tokens

main :: IO ()
main = do
    source <- readSource "/home/yorkin/CWC/test/pipeline/test2.l"
    pPrint $ Typer.typing $ parse source 
