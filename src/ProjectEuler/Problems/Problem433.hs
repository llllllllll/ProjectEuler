{-# LANGUAGE BangPatterns #-}
-- NOT YET COMPLETED.
module Problems.Problem433
    ( problem433
    , eTest
    , s
    ) where

problem433 = 0

eTest = e

e :: Integral a => a -> a -> Int
e a b = e' a b 0
  where
      e' !a !b !c
             | a `rem` b == 0 = c + 1
             | otherwise = e' b (a `rem` b) (c + 1)

s :: Integral a => a -> Int
s n = sum [e a b | a <- [1..n], b <- [1..n]]
 