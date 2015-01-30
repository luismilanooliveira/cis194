{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = 0:1:zipWith (+) fibs2 (tail fibs2)

data Stream a = a :- Stream a
infixr  :-

streamToList :: Stream a -> [a]
streamToList (z:- s) = z:streamToList s

instance Show a => Show (Stream a) where
  show s = show $ take 50 $ streamToList s

streamRepeat :: a -> Stream a
streamRepeat z = z:-streamRepeat z

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (z:- s) = f z:- streamMap f s

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f s = s:- streamFromSeed f (f s)

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (z :- s1) (y :- s2) = z :- y :- interleaveStreams s1 s2

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) $ interleaveStreams (streamRepeat 1)
  (interleaveStreams (streamRepeat 2) (streamFromSeed (+1) 3))

-- ex6
x :: Stream Integer
x = 0 :- (1 :- streamRepeat 0)

instance Num (Stream Integer) where
  fromInteger n = n :- streamRepeat 0
  negate = streamMap negate
  (+) (z:- zs) (y:- ys) = (z + y):- (+) zs ys
  (*) (z:- zs) s2@(y:- ys) = (z * y):- ((streamMap (*z) ys) + (zs * s2))

instance Fractional (Stream Integer) where
  (/) (z:- zs) (y:- ys) = q
    where q = (z `div` y) :- (streamMap (`div` y) (zs - q * ys))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

data Matrix = Matrix Integer Integer Integer Integer
  deriving Show

instance Num Matrix where
  (*) (Matrix a11 a12 a21 a22) (Matrix b11 b12 b21 b22) =
    Matrix (a11 * b11 + a12 * b21) (a11 * b12 + a12 * b22)
           (a21 * b11 + a22 * b21) (a21 * b12 + a22 * b22)

matrixF = (Matrix 1 1 1 0)

fibs4 :: Integer -> Integer
fibs4 0 = 0
fibs4 n = let (Matrix _ f _ _) = matrixF ^ n
           in f
