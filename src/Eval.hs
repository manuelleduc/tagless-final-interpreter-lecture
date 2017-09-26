module Eval where

import Lib

instance ExpSYM Int where
  lit a = a
  neg e = -e
  add e1 e2 = e1 + e2
