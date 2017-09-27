module Flatl (ti3_view, ti3_norm_view) where

import           Lib      (Exp (..), ti1, view)
import           PushNegl (push_neg)

flata :: Exp -> Exp
flata e@Lit{}              = e
flata e@Neg{}              = e
flata (Add (Add e1 e2) e3) = flata (Add e1 (Add e2 e3))
flata (Add e1 e2)          = Add e1 (flata e2)

norm :: Exp -> Exp
norm = flata . push_neg

ti3 :: Exp
ti3 = Add ti1 (Neg (Neg ti1))

ti3_view :: String
ti3_view = view ti3

ti3_norm :: Exp
ti3_norm = norm ti3

ti3_norm_view :: String
ti3_norm_view = view ti3_norm
