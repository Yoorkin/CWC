module Parsing.ParserCombinator
  ( result,
    error,
    item,
    -- seq,
    bind,
    satisfy,
    letter,
    char,
    digit,
    upper,
    lower,
    string,
    integer,
    float,
    many,
    many1,
    parseBy,
    endOfInput,
    identifier,
    pos,
    sepBy,
    sepBy1,
    space,
    newline,
    tab,
    blank,
    plus,
    noneOf,
    oneOf,
    (<|>),
    ErrorType (..),
    Error (..),
    State (..),
    Parser,
    StringParser
  )
where

import Control.Applicative (Alternative, empty, (<|>))
import Data.Char (isDigit, isLower, isUpper)
import Data.Maybe
import Prelude hiding (error, seq)

data ErrorType a
  = UnexpectedItemError a
  | UnexpectedEndOfInputError
  | UnexpectedError
  deriving (Show, Eq)

data Error input = Error
  { errorOffset :: Int,
    errorType :: ErrorType input
  }
  deriving (Show, Eq)

data State input = State
  { stateConsumed :: Int,
    stateInput :: [input]
  }
  deriving (Show, Eq)

newtype Parser input result
  = Parser
      ( State input ->
        Either (Error input) (result, State input)
      )

instance Functor (Parser a) where
  fmap f (Parser x) = Parser $ \st -> do
    (x', st') <- x st
    return (f x', st')

instance Applicative (Parser a) where
  pure = result
  (Parser f) <*> (Parser x) = Parser $ \st -> do
    (f', st') <- f st
    (x', st'') <- x st'
    return (f' x', st'')

plus :: Parser input a -> Parser input a -> Parser input a
plus (Parser p) (Parser q) = Parser $ \st ->
  let x = p st
   in case x of
        Right _ -> x
        Left _ -> q st

instance Alternative (Parser a) where
  empty = error UnexpectedError
  (<|>) = plus

instance Monad (Parser a) where
  (>>=) = bind

result :: r -> Parser input r
result v = Parser $ \st -> return (v, st)

error :: ErrorType a -> Parser a b
error err = Parser $ \st ->
  Left $
    Error
      { errorOffset = stateConsumed st - 1,
        errorType = err
      }

item :: Parser input input
item = Parser $ \st -> case stateInput st of
  [] -> apply (error UnexpectedError) st
  (x : xs) ->
    Right
      ( x,
        st
          { stateInput = xs,
            stateConsumed = stateConsumed st + 1
          }
      )

-- seq :: Parser input a -> Parser input b -> Parser input (a,b)
-- p `seq` q = \st -> do st <- p inp
--                        (w, inp'') <- q inp'
--                        return ((v,w), inp'')

apply :: Parser input result -> State input -> Either (Error input) (result, State input)
apply (Parser p) = p

bind :: Parser input a -> (a -> Parser input b) -> Parser input b
p `bind` f = Parser $
  \st -> do
    (v, st') <- apply p st
    apply (f v) st'

satisfy :: (input -> Bool) -> Parser input input
satisfy p = item `bind` \x -> if p x then result x else error (UnexpectedItemError x)

many :: Parser input a -> Parser input [a]
many p = (:) <$> p <*> many p <|> return []

many1 :: Parser input a -> Parser input [a]
many1 p = (:) <$> p <*> many p

sepBy, sepBy1 :: Parser input a -> Parser input b -> Parser input [a]
p `sepBy` d = (:) <$> p <*> many (d *> p) <|> return []
p `sepBy1` d = (:) <$> p <*> many (d *> p)

option :: a -> Parser input a -> Parser input a
option v p = p <|> return v

optionMaybe :: Parser input a -> Parser input (Maybe a)
optionMaybe p = Just <$> p <|> return Nothing

between :: Parser i a -> Parser i b -> Parser i c -> Parser i b
between l p r = l *> p <* r

endOfInput :: Parser input ()
endOfInput = Parser $ \st -> case stateInput st of
  [] -> return ((), st)
  _ -> apply (error UnexpectedEndOfInputError) st

pos :: Parser input Int
pos = Parser $ \st -> return (stateConsumed st, st)

parseBy :: Parser input result -> [input] -> Either (Error input) result
parseBy (Parser p) inp =
  let st =
        State
          { stateConsumed = 0,
            stateInput = inp
          }
   in case p st of
        Left err -> Left err
        Right (v, _) -> Right v

type StringParser a = Parser Char a

char :: Char -> StringParser Char
char x = satisfy (== x)

digit, lower, upper, letter, space, newline, tab, blank :: StringParser Char
digit = satisfy isDigit
lower = satisfy isLower
upper = satisfy isUpper
letter = lower <|> upper
space = char ' '
tab = char '\t'
newline = char '\n'
blank = space <|> tab <|> newline

string :: String -> StringParser String
string [] = return ""
string (x : xs) = (:) <$> char x <*> string xs

identifier :: StringParser String
identifier = (:) <$> letter <*> many (digit <|> letter <|> char '_')

integer :: StringParser Int
integer = read <$> many1 digit

float :: StringParser Float
float = do
  a <- many1 digit
  _ <- char '.'
  c <- many1 digit
  return (read $ a ++ "." ++ c)

oneOf :: [Char] -> StringParser Char
oneOf xs =
  item `bind` \x ->
    if x `elem` xs
      then result x
      else error (UnexpectedItemError x)

noneOf :: [Char] -> StringParser Char
noneOf xs =
  item `bind` \x ->
    if x `notElem` xs
      then result x
      else error (UnexpectedItemError x)
