{-# LANGUAGE FlexibleInstances #-}

module CalcVar where

import Calc
import qualified Data.Map as M

class HasVars a where
  var :: String -> a

data VarExprT = Lit Integer
           | Add VarExprT VarExprT
         | Mul VarExprT VarExprT
         | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = Lit
  add = Add
  mul = Mul

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit n = (\_ -> Just n)
  add e1 e2 = (\m -> applyMaybe (+) (e1 m) (e2 m))
  mul e1 e2 = (\m -> applyMaybe (*) (e1 m) (e2 m))

applyMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
applyMaybe f (Just n) (Just m) = Just (n `f` m)
applyMaybe _ _ _               = Nothing

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
