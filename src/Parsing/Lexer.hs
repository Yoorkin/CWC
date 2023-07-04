module Parsing.Lexer(tokenize, Token(..)) where
import Parsing.ParserCombinator hiding (identifier)
import Data.Functor

data Token
    = TokenIdent String
    | TokenKeyword String
    | TokenOp String
    | TokenInt Int
    | TokenFloat Float
    | TokenBool Bool
    | TokenString String
    | TokenChar Char
    | TokenEof
    deriving(Show,Eq)

merge :: [a] -> a -> [a]
merge x y = x ++ [y]

skip :: Parser Char [Char]
skip = many blank

eof :: Parser input Token
eof = endOfInput $> TokenEof

stringLiteral :: Parser Char [Char]
stringLiteral = char '"' *> many (noneOf "\"") <* char '"'

charLiteral :: Parser Char Char
charLiteral = char '\'' *> noneOf "'" <* char '\''

identifier :: Parser Char [Char]
identifier = (:) <$> (char '_' <|> letter) <*> many (digit <|> char '_' <|> letter)

processOperators :: [String] -> Parser Char Token
processOperators = foldl1 (<|>) . fmap (fmap TokenOp . string)

keywords :: [String]
keywords = [
    "let", "rec", "and", "fun", "case", "of", "data",
    "type", "try", "with", "raise", "if", "then", "else"
    ]

tokens :: Parser Char Token
tokens = do 
           id <- identifier
           case id of 
            "true" -> return $ TokenBool True
            "false" -> return $ TokenBool False
            _ -> return $ if id `elem` keywords 
                            then TokenKeyword id
                            else TokenIdent id                      
        <|> TokenString <$> stringLiteral
        <|> TokenChar <$> charLiteral
        <|> TokenFloat <$> float
        <|> TokenInt <$> integer
        <|> processOperators [
            "=>","->",
            ">=", "<=", "=", "<>", ",", "[", "]","{","}",
            "+", "-", "*", "/", "(", ")", "<", ">", ":", "|"
        ]

lexer :: StringParser [Token]
lexer = merge <$> many (skip *> tokens) <*> (skip *> eof)

tokenize :: String -> Either (Error Char) [Token]
tokenize = parseBy lexer
