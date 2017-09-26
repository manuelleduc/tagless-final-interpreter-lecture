{-# LANGUAGE FlexibleInstances #-}

module Sem.PrettyPrinter where

import           Exp.Mul
import           Exp.Sum

instance ExpSYM String where
  lit = show
  neg e = "(-" ++ e ++ ")"
  add e1 e2 = "(" ++ e1 ++ " + " ++ e2 ++ ")"

instance MulSYM String where
  mul e1 e2 = "(" ++ e1 ++ " * " ++ e2 ++ ")"

view :: String -> String
view = id
