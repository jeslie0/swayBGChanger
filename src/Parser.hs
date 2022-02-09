{-# LANGUAGE LambdaCase #-}
module Parser
where



import           Control.Applicative
import Data.Char
    ( isAlpha, isDigit, isAlphaNum, isLower, isSpace, isUpper )
import           Data.List

-- * Types

newtype Parser a = Parser {parse :: String -> Maybe (a, String)}

instance Functor Parser where
  f `fmap` pg = Parser $ \input ->
    case parse pg input of
      Nothing          -> Nothing
      Just (a, string) -> Just (f a, string)

instance Applicative Parser where
  pure a = Parser $ \input -> Just (a, input)
  pf <*> pa = Parser $ \input -> case parse pf input of
    Nothing       -> Nothing
    Just (h, out) -> parse (fmap h pa) out

instance Monad Parser where
  pf >>= g = Parser $ \input -> case parse pf input of
    Nothing       -> Nothing
    Just (a, out) -> parse (g a) out

instance Alternative Parser where
  empty = Parser $ const Nothing

  pf <|> pg = Parser $ \input -> case parse pf input of
    Nothing       -> parse pg input
    Just (a, out) -> Just (a, out)

-- * List functions

initialSubSeq :: Eq a => [a] -> [a] -> Bool
initialSubSeq [] [] = True
initialSubSeq [] (y : ys) = True
initialSubSeq (x : xs) [] = False
initialSubSeq (x : xs) (y : ys)
  | x == y = initialSubSeq xs ys
  | otherwise = False

breakAtSubSeq :: Eq a => [a] -> [a] -> ([a], [a])
breakAtSubSeq xs [] = ([], [])
breakAtSubSeq xs (y : ys)
  | xs `initialSubSeq` (y : ys) = ([], y : ys)
  | otherwise =
    let (a, b) = breakAtSubSeq xs ys
     in (y : a, b)

-- * Basic functions

-- | Gives the first character of a string, otherwise outputs nothing
-- >>> parse item "hello"
-- Just ('h',"ello")
item :: Parser Char
item = Parser $ \case
  ""       -> Nothing
  (x : xs) -> Just (x, xs)

-- | Takes a character predicate and returns the parser for single
-- characters that satisfy the predicate p.
sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x
    then return x
    else empty

-- | Parses a string to a digit and the remainder.
digit :: Parser Char
digit = sat isDigit

-- | Parses a string to a lower character and the remainder.
lower :: Parser Char
lower = sat isLower

-- | Parses a string to an upper character and the remainder.
upper :: Parser Char
upper = sat isUpper

-- | Parses a string to a letter and the remainder.
letter :: Parser Char
letter = sat isAlpha

-- | Parses a string to an alphanumeric character and the remainder.
alphanum :: Parser Char
alphanum = sat isAlphaNum

-- | Takes a character and makes a parser for that character.
char :: Char -> Parser Char
char x = sat (== x)

newline :: Parser Char
newline = char '\n'

-- | Takes a string and returns the parser with the string itself as
-- the result value.
-- >>> parse (string "abc") "abcdef"
-- Just ("abc","def")
string :: String -> Parser String
string "" = return ""
string (x : xs) = do
  char x
  string xs
  return (x : xs)

-- | A parser that parses everything up to a given string.
untilSymbol :: String -> Parser String
untilSymbol str = Parser $ \input ->
  if str `isInfixOf` input
    then Just $ breakAtSubSeq str input
    else Nothing

nat :: Parser Int
nat = do
  xs <- some digit
  return (read xs)

int :: Parser Int
int =
  ( do
      char '-'
      n <- nat
      return (- n)
  )
    <|> nat

restOfLine :: Parser String
restOfLine = many $ sat (/= '\n')




-- * Space handling


isSpace' :: Char -> Bool
isSpace' '\n' = False
isSpace' x    = isSpace x

-- | A parser that strips the initial whitespace of a string, and
-- gives the rest as the output.
-- >>> parse space "    hi"
-- Just ((),"hi")
space :: Parser ()
space = do
  many (sat isSpace')
  return ()

-- | Ignores spaces before and after applying a parser for a token.
token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

-- | Removes initial whitespace
-- >>> parse natural "     123  8  "
-- Just (123,"8  ")
natural :: Parser Int
natural = token nat

word :: Parser String
word = do
  space
  many $ sat (/= ' ')

-- | An integer parser that ignores whitespace.
integer :: Parser Int
integer = token int

-- | Takes a string and parses it while ignoring whitespace.
symbol :: String -> Parser String
symbol xs = token (string xs)

-- | Parser for lists of natural numbers, ignoring whitespace.
-- >>> parse nats "  [1, 2,   3]"
-- Just ([1,2,3],"")
nats :: Parser [Int]
nats = do
  symbol "["
  n <- natural
  ns <- many $ symbol "," >> natural
  symbol "]"
  return (n : ns)

-- | Parser for lists of integers, ignoring whitespace.
-- >>> parse ints "   [-1, 9  , -22]  "
-- Just ([-1,9,-22],"")
ints :: Parser [Int]
ints = do
  symbol "["
  n <- integer
  ns <- many $ symbol "," >> integer
  symbol "]"
  return (n : ns)

-- * Monadic stuff

-- | Returns the remaining string to be parsed.
get :: Parser String
get = Parser $ \input -> Just (input, input)

-- | Replace the remaining string to be parsed.
put :: String -> Parser ()
put string = Parser $ const $ Just ((), string)
