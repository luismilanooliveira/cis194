module Calc where

import ExprT
import Parser


eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add n m) = (eval n) + (eval m)
eval (Mul n m) = (eval n) * (eval m)

evalStr :: String -> Maybe Integer
evalStr expr = case (parseExp Lit Add Mul expr) of
                   (Just e) -> Just (eval e)
                   Nothing  -> Nothing

class Expr e where
  lit :: Integer -> e
  add :: e -> e -> e
  mul :: e -> e -> e

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

reify :: ExprT -> ExprT
reify = id

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Show, Eq, Ord)
newtype Mod7   = Mod7 Integer deriving (Show, Eq)

instance Expr MinMax where
  lit = MinMax
  add = max
  mul = min

instance Expr Mod7 where
  lit x = Mod7 (x `mod` 7)
  add (Mod7 x) (Mod7 y) = lit (x+y)
  mul (Mod7 x) (Mod7 y) = lit (x*y)


testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3* -4) + 5"

testInteger  = testExp :: Maybe Integer
testBool     = testExp :: Maybe Bool
testMM       = testExp :: Maybe MinMax
testSat      = testExp :: Maybe Mod7
