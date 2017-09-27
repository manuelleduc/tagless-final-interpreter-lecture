{-# LANGUAGE FlexibleInstances #-}

module FlatF () where

import           Exp.Sum (ExpSYM (..))

data Ctx e = LCA e | NonLCA


instance ExpSYM repr => ExpSYM (Ctx repr -> repr) where
  lit n NonLCA  = lit n
  lit n (LCA e) = add (lit n) e
  neg e NonLCA   = neg (e NonLCA)
  neg e (LCA e3) = add (neg (e NonLCA)) e3
  add e1 e2 ctx = e1 (LCA (e2 ctx))
