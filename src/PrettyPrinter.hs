{-# LANGUAGE FlexibleInstances #-}

module PrettyPrinter where

import Lib

instance ExpSYM String where
  lit = show
  neg e = "(-" ++ e ++ ")"
  add e1 e2 = "(" ++ e1 ++ " + " ++ e2 ++ ")"
