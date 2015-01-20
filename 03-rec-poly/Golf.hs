{-# OPTIONS_GHC -Wall -XTupleSections #-}

module Golf where

import Data.List (transpose)

skips :: [a] -> [[a]]
skips xs = zipWith p [1..length xs] $ repeat xs

p :: Int -> [a] -> [a]
p n = foldr s [] . zip [1..]
  where s (i,x) a = if rem i n == 0 then x:a else a

localMaxima :: [Integer] -> [Integer]
localMaxima xs = m . f . z $ xs
  where m    = map (\(_,b,_) -> b)
        f    = filter (\(a,b,c) -> b > a && b > c)
        z l  = zip3 l (tail l) (drop 2 l)


histogram :: [Integer] -> String
histogram is = (unlines . filter (not . all (== ' ')) . transpose
  . map snd . counter $ is) ++ "==========\n0123456789\n"

counter :: [Integer] -> [(Integer, String)]
counter = foldr insert ls
  where ls = map (,(replicate 10 (' '))) [0..9]

insert :: Integer -> [(Integer, String)] -> [(Integer, String)]
insert _ []             = []
insert _ ((_, []):_)    = []
insert n ((k,x:xs):xss) = if n == k
                             then (k,xs ++ "*"):xs
                             else (k,x:xs):insert n xss
