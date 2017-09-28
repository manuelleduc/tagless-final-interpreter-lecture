{-# LANGUAGE GADTs #-}

module TTIF () where

import TTFdB (R(..))


data IR h t where
  INT :: Int -> IR h Int
  Add :: IR h Int -> IR h Int -> IR h Int
  Var :: h t -> IR h t
  Lam :: (IR h t1 -> IR h t2) -> IR h (t1 -> t2)
  App :: IR h (t1 -> t2) -> IR h t1 -> IR h t2

ti1 :: IR h Int
ti1 = Add (INT 1) (INT 2)

ti2 :: IR h (Int -> Int)
ti2 = Lam (\x -> Add x x)

ti3 :: IR h ((Int -> Int) -> Int)
ti3 = Lam (\x -> Add (App x (INT 1)) (INT 2))



evall :: IR R t -> t
evall (INT n) = n
evall (Add e1 e2) = evall e1 + evall e2
evall (Var v) = unR v
evall (Lam b) = \x -> evall (b . Var . R $ x)
evall (App e1 e2) = evall e1 (evall e2)
