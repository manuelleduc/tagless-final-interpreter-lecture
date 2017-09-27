module TTFdB (view, eval, td1, td2o, td2o', td3) where

class Symantics repr where
  int :: Int -> repr h Int
  add :: repr h Int -> repr h Int -> repr h Int

  z :: repr (a, h) a
  s :: repr h a -> repr (any, h) a
  lam :: repr (a, h) b -> repr h (a -> b)
  app :: repr h (a -> b) -> repr h a -> repr h b

class MulSYM repr where
  mult :: repr Int -> repr Int -> repr Int

class BoolSYM repr where
  bool :: Bool -> repr Bool
  leq :: repr Int -> repr Int -> repr Bool
  if_ :: repr Bool -> repr a -> repr a -> repr a

class FixSYM repr where
  fix :: (repr a -> repr a) -> repr a

td1 :: Symantics repr => repr h Int
td1 = add (int 1) (int 2)

td2o :: Symantics repr => repr (Int, h) (Int -> Int)
td2o = lam (add z (s z))

td2o' :: Symantics repr => repr (Int, h) Int
td2o' = app td2o (int 3)


--td3 :: Symantics repr => repr h ((Int -> Int) -> Int)
td3 :: Symantics repr => repr h ((Int -> Int) -> Int)
td3 = lam (add (app z (int 1)) (int 2))


tpow = lam (\x -> fix (\self -> lam (\n ->
                        if_ (leq n (int 0)) (int 1)
                            (mul x (app self (add n (int (-1))))))))


newtype R h a = R { unR :: h -> a }

instance Symantics R where
  int x = R $ const x
  add e1 e2 = R $ \h -> unR e1 h + unR e2 h
  z = R fst
  s v = R $ \(_,h) -> unR v h
  lam e = R $ \h x -> unR e (x, h)
  app e1 e2 = R $ \h -> (unR e1 h) (unR e2 h)

eval :: R () a -> a
eval e = unR e ()



newtype S h a = S { unS :: Int -> String }

instance Symantics S where
  int x = S $ \_ -> show x
  add e1 e2 = S $ \h -> "(" ++ unS e1 h ++ " + " ++ unS e2 h ++ ")"
  z = S $ \h -> "x" ++ show (h-1)
  s v = S $ \h -> unS v (h-1)
  lam e = S $ \h -> let x = "x" ++ show h
                    in "(\\" ++ x ++ " -> " ++ unS e (h+1) ++ ")"
  app e1 e2 = S $ \h -> "(" ++ unS e1 h ++ " " ++ unS e2 h ++ ")"

view :: S h a -> String
view e = unS e 0
