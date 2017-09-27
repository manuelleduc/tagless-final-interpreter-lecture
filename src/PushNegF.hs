{-# LANGUAGE FlexibleInstances #-}

module PushNegF (push_neg) where

import Exp.Sum (ExpSYM(..))
import Exp.Mul (MulSYM(..))

data Ctx = Pos | Neg

instance ExpSYM repr => ExpSYM (Ctx -> repr) where
  lit n Pos = lit n
  lit n Neg = neg (lit n)
  neg e Pos = e Neg
  neg e Neg = e Pos
  add e1 e2 ctx = add (e1 ctx) (e2 ctx)

instance MulSYM repr => MulSYM (Ctx -> repr) where
  mul e1 e2 Pos = mul (e1 Pos) (e2 Pos)
  mul e1 e2 Neg = mul (e1 Pos) (e2 Neg)


push_neg :: (Ctx -> t) -> t
push_neg e = e Pos
