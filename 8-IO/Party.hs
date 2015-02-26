{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Party where

import Data.Monoid
import Data.Tree
import Data.List (sort)

import Employee
-- ex1
-- 1.1
glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) (f + empFun e)

-- 1.2
instance Monoid GuestList where
  mempty = GL [] 0
  (GL es1 f1) `mappend` (GL es2 f2) = GL (es1 `mappend` es2) (f1 + f2)

-- 1.3
moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- ex2
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node root forest) = f root (map (treeFold f) forest)

-- ex3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e gls =
  let glWithBosses    = mconcat $ map fst gls
      glWithoutBosses = mconcat $ map snd gls
      withoutE        = moreFun glWithBosses glWithoutBosses
      withE           = glCons e withoutE
  in (withE, withoutE)

-- ex4
maxFun :: Tree Employee -> GuestList
maxFun tree = let pair = treeFold nextLevel tree
          in moreFun (fst pair) (snd pair)

format :: GuestList -> String
format (GL emps f)  = unlines $ ("Total Fun: " ++ show f):sort names
  where names = map empName emps


main :: IO ()
main = do
  company <- readFile "company.txt"
  let guestList = maxFun . read $ company
  putStrLn . format $ guestList

-- Test employees
-- korey    = Emp "Korey" 100
-- bill     = Emp "Bill" 5
-- wenjie   = Emp "Wenjie" 3
-- kevin    = Emp "Kevin" 5
-- bulman   = Emp "Bulman" 6
-- hartsock = Emp "Hartsock" 38
-- pat      = Emp "Pat" 500
-- alaina   = Emp "Alaina" 40
-- carly    = Emp "Carly" 100
-- zoe      = Emp "Zoe" 50
-- rick     = Emp "Rick" 6
-- mikah    = Emp "Mikah" 38
-- spencer  = Emp "Spencer" 100
-- sampson  = Emp "Sampson" 3
