{-# LANGUAGE BangPatterns #-}
module Utils.Number
    ( divisors
    , r_divisors
    , factorial
    , num_divisors
    , partitions
    , exp_by_sq
    , hyper_exp
    , mod_exp
    , nCr
    , nPr
    , is_int
    , is_square
    , is_cube
    , euler_totient
    , euclid_gcd
    ) where

import Data.Bits (Bits(..),shift)
import Data.List (genericLength,nub,(\\))
import Utils.Prime (prime_factors_mult)

-- A list of all divisors of n.
divisors :: Integral a => a -> [a]
divisors n = 1:(concat
                [[x,n`div`x]
                     | x <- [2..floor (sqrt (fromIntegral (n)))],
                            n `rem` x == 0] \\
                (if is_square n then [floor (sqrt (fromIntegral n))]
                 else []))

-- A list of all divisors of n in reverse order.
r_divisors :: Integral a => a -> [a]
r_divisors n = [x | x <- [n, n-1..1], n `rem` x == 0]

-- factorial n = n!.
factorial :: Integral a => a -> a
factorial n = product [2..n]

-- The number of divisors of n.
num_divisors :: Integral a => a -> a
num_divisors n = (genericLength . divisors) n

-- The number of partitions in n.
partitions n = c n n
  where
      c 0 _ = 1
      c _ 0 = 1
      c n m = sum $ map (\x -> l!!(n-x)!!min (n-x) x) [1..m]
      l = [[c n m | m <- [0..n]] | n<-[0..]]

-- Exponentiation by squares in O(log b).
exp_by_sq :: (Num a, Integral b) => a -> b -> a
exp_by_sq a b
    | b == 0 = 1
    | b == 1 = a
    | even b = exp_by_sq (a*a) (b`div`2)
    | odd b = a*(exp_by_sq (a) (b-1))

-- Tetaration or power tower for a b.
hyper_exp a b
    | b == 1 = a
    | otherwise = exp_by_sq a (hyper_exp a (b-1))

mod_exp :: (Integral a, Bits a) => a -> a -> a -> a
mod_exp a b c = exp_mod' a b c 1
  where
      exp_mod' a b c s
          | b == 0 = s
          | odd b = exp_mod' (a^2 `rem` c) (shift b (0-1)) c ((s * a) `mod` c)
          | otherwise = exp_mod' (a^2 `rem` c) (shift b (0-1)) c s

-- Number of combinations of n choose r.
nCr :: Integral a => a -> a -> a
n `nCr` r = factorial n `div` (factorial r * factorial (n-r))

-- Number of permutation of n pick r.
nPr :: Integral a => a -> a -> a
n `nPr` r = factorial n `div` factorial (n-r)

-- Returns if n is an Integral to 7 decimal places.
is_int :: RealFrac a => a -> Bool
is_int n = (round $ 10^(fromIntegral 7)*(n-(fromIntegral $ round n))) == 0

-- Returns True if n is a perfect square.
is_square :: Integral a => a -> Bool
is_square n = ((round (fromIntegral (n)**(0.5))) ^2 == n)

-- Returns True if n is a perfect cube.
is_cube :: Integral a => a -> Bool
is_cube n = (round (fromIntegral (n)**(1/3))) ^3 == n

-- The size of the list of numbers coprime to n.
euler_totient :: Integer -> Integer
euler_totient 1 = 1
euler_totient n = product
                  [(p - 1) * p ^ (c - 1) | (p, c) <- prime_factors_mult n]

-- Euclid's Algorithm with forced strictness.
euclid_gcd :: Integral a => a -> a -> a
euclid_gcd (!a) (!b)
    | a `rem` b == 0 = b
    | otherwise = euclid_gcd b (a `rem` b)
