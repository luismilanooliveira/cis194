{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}


module JoinList where

import Data.Monoid
import Sized
import Scrabble
import Buffer
import Editor

data JoinList m a =  Empty
                    | Single m a
                    | Append m (JoinList m a) (JoinList m a)
                  deriving (Show, Eq)

tag :: Monoid m => JoinList m a -> m
tag (Single m _)   = m
tag (Append m _ _) = m
tag Empty          = mempty

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
jl1 +++ jl2 = Append (tag jl1 `mappend` tag jl2) jl1 jl2

-- returns the ith element of a list, if it exists
(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:_) !!? 0         = Just x
(_:xs) !!? i         = xs !!? (i-1)

-- convert a join list into a list
jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- helper function, returns the annotated size of a join list
jlSize :: (Sized b, Monoid b) => JoinList b a -> Int
jlSize = getSize . size . tag

-- returns the ith element of the join list if it exists
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty                           = Nothing
indexJ i jl | i > jlSize jl              = Nothing
indexJ 0 (Single _ a)                    = Just a
indexJ i (Append _ l r )
  | i < jlSize l           = indexJ i l
  | otherwise              = indexJ (i - jlSize l) r
indexJ _ _                               = Nothing

-- jl list to test the functions implemented
jlist :: JoinList Size Char
jlist = (Append (Size 3)
          (Append (Size 2) (Single (Size 1) 'a') (Single (Size 1) 'b'))
          (Single (Size 1) 'c'))

-- drops the first n elements from a JoinList
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 0 jl                = jl
dropJ _ Empty             = Empty
dropJ _ (Single _ _)      = Empty
dropJ n jl@(Append _ l r)
  | n >= (jlSize jl) = Empty
  | n < jlSize l    = (dropJ n l) +++ r
  | otherwise       = dropJ (n - jlSize l) r

-- takes the first n elements from a JoinList
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n _ | n <= 0 = Empty
takeJ n jl@(Append _ l r)
  | n >= (jlSize jl) = jl
  | n <= jlSize l    = takeJ n l
  | otherwise       = l +++ takeJ (n - jlSize l) r
takeJ _ jl        = jl

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

type JLBuffer = JoinList (Score, Size) String

instance Buffer JLBuffer where
  -- toString :: JLBuffer -> String
  toString = unlines . jlToList

  -- fromString :: String -> JLBuffer
  fromString = foldr (+++) Empty . stringToJl
    where stringToJl = map (\s -> Single (scoreString s, Size 1) s) . lines

  -- line :: Int -> JLBuffer -> Maybe String
  line = indexJ

  -- replaceLine :: Int -> String -> JLBuffer -> JLBuffer
  replaceLine n ln buf = takeJ n buf +++ fromString ln +++ dropJ (n+1) buf

  -- numLines :: JLBuffer -> Int
  numLines = getSize . snd . tag

  -- value :: JLBuffer -> Int
  value = getScore . fst . tag

-- JoinList-based Buffer Editor

main = runEditor editor buffer
  where buffer = fromString (unlines
                    [ "This buffer is for notes you don't want to save, and for"
                    , "evaluation of steam valve coefficients."
                    , "To load a different file, type the character L followed"
                    , "by the name of the file."
                    ]) :: JLBuffer
