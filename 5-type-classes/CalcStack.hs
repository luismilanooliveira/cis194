{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module CalcStack where

import Calc
import StackVM
import Parser

instance Expr Program where
  lit n = [PushI n]
  add p1 p2  = p1 ++ p2 ++ [Add]
  mul p1 p2  = p1 ++ p2 ++ [Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

