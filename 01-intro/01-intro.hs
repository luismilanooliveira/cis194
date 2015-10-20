{-# OPTIONS_GHC -Wall #-}

-- Credit Card Validation

-- ex1 - conversion of an integer to a list of its digits

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | n < 10    = [n]
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev


-- ex2 - doubling every other element, starting from the right side of the list
-- doubleEveryOther [8,7,6,5] == [16,7,12,5]
-- doubleEveryOther [1,2,3]   == [1,4,3]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleHelper . reverse
  where doubleHelper (x:y:xs) = (x:y*2:doubleHelper xs)
        doubleHelper xs       = xs

-- ex3 - summing up all the digits from a list of numbers
sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigitsRev

-- ex4 - validating a credit card number, using all the functions defined before
validate :: Integer -> Bool
validate number = let digits = toDigits number
                      doubled = doubleEveryOther digits
                      summed = sumDigits doubled
                      in summed `mod` 10 == 0
--
-- Alternate Versions
-- point-free version
-- validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits

-- using elixir's pipeling operator
-- actually, it works like a backwards function composition
-- (|>) :: (a -> b) -> (b -> c) -> a -> c
-- (|>) :: a -> (a -> b) -> b
-- (|>) a f = f a
-- infixl 9 |>

-- validate'' :: Integer -> Bool
-- validate'' n = n |> toDigits |> doubleEveryOther |> sumDigits |> (`mod` 10) |> (== 0)


-- tower of Hanoi exercises
-- using ADT for easy of using the repl to debug the code
data Peg = A | B | C | D deriving Show

type Move = (Peg, Peg)

-- with three pegs
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n == 1    = [(a,b)]
  | otherwise = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

-- optional exercise - with 4 pegs (cf. frame stewart algorithm)

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n p1 p2 p3 p4
  | n == 1 = [(p1,p2)]
  | otherwise =    hanoi4 (n `div` 2) p1 p3 p2 p4
                ++ hanoi4 (n - (n `div` 2)) p1 p2 p4 p3
                ++ hanoi4 (n `div` 2) p3 p2 p1 p4
