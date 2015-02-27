{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import Control.Applicative

import Data.Char

-- A parser for a value of type a is a function which takes a String
-- representing the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)


{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- ex1
--
first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Functor Parser where
  fmap f (Parser p) = Parser $ fmap (first f) . p

-- ex2
--
instance Applicative Parser where
  pure a  = Parser (\_ -> Just (a, []))
  p1 <*> p2 = Parser (\s -> case runParser p1 s of
                      Nothing -> Nothing
                      Just (function, r) ->
                        case runParser p2 r of
                             Nothing -> Nothing
                             Just (value, rest) -> Just (function value, rest))

-- testing the applicative instance
type Name = String
data Employee = Emp { name :: Name, phone :: String }
  deriving Show

-- idiotic parsers, just for testings
parseName :: Parser Name
parseName = Parser (\s -> Just (take 3 s, drop 3 s))

parsePhone :: Parser String
parsePhone = Parser (\s -> Just (take 3 s, drop 3 s))

parseFail :: Parser String
parseFail = Parser (\_ -> Nothing)

-- returns a Employee from a String
parseEmployee :: Parser Employee
parseEmployee = Emp <$> parseName <*> parsePhone

-- ex3
--
-- parses "ab" and returns (('a','b'), rest)
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

-- parses "ab" and returns ((), rest)
abParser_ :: Parser ()
abParser_ =  (\_ -> pure ()) <$> char 'a' <*> char 'b'

space :: Parser Char
space = char ' '

intPair :: Parser [Integer]
intPair = result <$> posInt <*> space <*> posInt
  where result a b c = [a, c]

-- ex4
instance Alternative Parser where
  empty     = Parser (\s -> Nothing)
  p1 <|> p2 = Parser (\s -> runParser p1 s <|> runParser p2 s)

int_ :: Parser ()
int_ = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just ((), rest)
      where (ns, rest) = span isDigit xs

satisfy_ :: (Char -> Bool) -> Parser ()
satisfy_ p = Parser f
  where
    f [] = Nothing
    f (x:xs)
        | p x       = Just ((), xs)
        | otherwise = Nothing



intOrUpperCase :: Parser ()
intOrUpperCase = int_ <|> satisfy_ isUpper
