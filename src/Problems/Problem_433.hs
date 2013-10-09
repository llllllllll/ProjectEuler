{-# LANGUAGE BangPatterns #-}
-- NOT YET COMPLETED.
module Problems.Problem_433
    ( problem_433
    , e_test
    , s
    ) where

problem_433 = 0

e_test = e

e :: Integral a => a -> a -> Int
e a b = e' a b 0
  where
      e' !a !b !c
             | a `rem` b == 0 = c + 1
             | otherwise = e' b (a `rem` b) (c + 1)

s :: Integral a => a -> Int
s n = sum [e a b | a <- [1..n], b <- [1..n]]
