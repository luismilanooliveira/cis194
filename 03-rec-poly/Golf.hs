{-# OPTIONS_GHC -Wall -XTupleSections #-}

module Golf where

import Data.List (transpose)

skips :: [a] -> [[a]]
skips ls = zipWith p [1..length ls] $ repeat ls

p :: Int -> [a] -> [a]
p n = foldr s [] . zip [1..]
  where s (i,x) a = if rem i n == 0 then x:a else a

localMaxima :: [Integer] -> [Integer]
localMaxima xs = m . f . z $ xs
  where m    = map (\(_,b,_) -> b)
        f    = filter (\(a,b,c) -> b > a && b > c)
        z l  = zip3 l (tail l) (drop 2 l)


-- histogram :: [Integer] -> String
histogram is = (unlines . filter (not . all (== ' '))
  . transpose . render . counter $ is) ++ "==========\n0123456789\n"

ls = map (,(replicate 10 (-1))) [0..9]

counter :: [Integer] -> [(Integer, [Integer])]
counter = foldr insert ls

insert :: Eq a => a -> [(a, [a])] -> [(a, [a])]
insert _ []               = []
insert n ((k,x:xs):xss) = if n == k
                             then (k,xs ++ [n]):xss
                             else (k,x:xs):insert n xss
render :: [(Integer,[Integer])] -> [String]
render list = map (map intToStar) $ map snd list
  where intToStar c = if c == (-1)
                         then ' '
                         else '*'
