module Main where

import           Eval
import           Exp.Sum
import           PrettyPrinter

tf1 :: (ExpSYM a) => a
tf1 = add (lit 8) (neg (add (lit 1) (lit 2)))

main :: IO ()
main = do print (tf1 :: Int)
          putStrLn (tf1 :: String)
