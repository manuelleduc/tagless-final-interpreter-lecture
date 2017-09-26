module Exp.Sum where

class ExpSYM repr where
  lit :: Int -> repr
  neg :: repr -> repr
  add :: repr -> repr -> repr
