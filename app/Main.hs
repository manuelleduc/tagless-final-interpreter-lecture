module Main where

import           Exp.Mul           (tfm1, tfm2)
import           Exp.Sum           (tf1)
import           Sem.Eval
import           Sem.PrettyPrinter

main :: IO ()
main = do print (tf1 :: Int)
          putStrLn (tf1 :: String)
          print (tfm1 :: Int)
          putStrLn (tfm1 :: String)
          print (tfm2 :: Int)
          putStrLn (tfm2 :: String)
