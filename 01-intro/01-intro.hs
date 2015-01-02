-- credit card exercices
--
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
              | n <= 0 = []
              | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits n = myReverse (toDigitsRev n)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:ys) = 2*x:y:doubleEveryOther ys

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
                | x > 10 = sumDigits (toDigits x ++ xs)
                | otherwise = x + sumDigits xs

validate :: Integer -> Bool
validate number = 0 == (sumDigits (doubleEveryOther (toDigits number))) `mod` 10

-- hanoi tower
type Peg = String
type Move = (Peg, Peg)

-- with three pegs
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n p1 p2 p3
  | n == 1 = [(p1, p2)]
  | otherwise = hanoi (n-1) p1 p3 p2 ++ hanoi 1 p1 p2 p3 ++ hanoi (n-1) p3 p2 p1

-- optional exercise - with 4 pegs (cf. frame stewart algorithm)
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n p1 p2 p3 p4
  | n == 1 = [(p1, p2)]
  | otherwise = hanoi4 (n `div` 2) p1 p3 p2 p4 ++
      hanoi4 (n - (n `div` 2)) p1 p2 p4 p3 ++
        hanoi4 (n `div` 2) p3 p2 p1 p4

