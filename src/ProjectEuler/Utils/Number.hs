-- |
-- Module      : ProjectEuler.Utils.Number
-- Copyright   : Joe Jevnik 2013
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- Functions dealing with numbers and properties of numbers.

{-# LANGUAGE BangPatterns #-}

module ProjectEuler.Utils.Number
    ( divisors     -- :: Integral a => a -> [a]
    , rDivisors    -- :: Integral a => a -> [a]
    , numDivisors  -- :: (Integral a,Integral b) => a -> b
    , factorial    -- :: Integral a => a -> a
    , partitions   -- :: Integral a => Int -> a
    , expBySq      -- :: (Num a,Integral b) => a -> b -> a
    , hyperExp     -- :: (Eq a, Integral b, Num a) => b -> a -> b
    , modExp       -- :: (Integral a, Bits a) => a -> a -> a -> a
    , nCr          -- :: :: Integral a => a -> a -> a
    , nPr          -- :: Integral a => a -> a -> a
    , isInt        -- :: RealFrac a => a -> Bool
    , isSquare     -- :: Integral a => a -> Bool
    , isCube       -- :: Integral a => a -> Bool
    , eulerTotient -- :: Integer -> Integer
    , euclidGCD    -- :: Integral a => a -> a -> a
    ) where

import Data.Bits               (Bits(..),shift)
import Data.List               (genericLength,nub,(\\))
import ProjectEuler.Utils.Prime (primeFactorsMult)

-- | A list of all divisors of n.
divisors :: Integral a => a -> [a]
divisors n = n:1:(concat [[x,n`div`x] | x <- [2..floor $ sqrt $ fromIntegral n]
                         , n `rem` x == 0]
                  \\ if isSquare n
                       then [floor $ sqrt $ fromIntegral n]
                       else [])

-- | A list of all divisors of n in reverse order.
rDivisors :: Integral a => a -> [a]
rDivisors = reverse . divisors


-- | The number of divisors of n.
numDivisors :: (Integral a,Integral b) => a -> b
numDivisors = genericLength . divisors

-- | factorial n = n!.
factorial :: Integral a => a -> a
factorial n = product [2..n]

-- | The number of partitions in n.
partitions :: Integral a => Int -> a
partitions n = c n n
  where
      c 0 _ = 1
      c _ 0 = 1
      c n m = sum $ map (\x -> l !! (n-x) !! min (n - x) x) [1..m]
      l = [[c n m | m <- [0..n]] | n <- [0..]]

-- | Exponentiation by squares in O(log b).
expBySq :: (Num a,Integral b) => a -> b -> a
expBySq _ 0 = 1
expBySq a 1 = a
expBySq a b
    | even b = expBySq (a * a) (b `div` 2)
    | odd b = a * (expBySq (a) (b - 1))

-- | Tetaration or power tower for a b.
hyperExp :: (Eq a, Integral b, Num a) => b -> a -> b
hyperExp a 1 = a
hyperExp a b = expBySq a (hyperExp a (b - 1))

-- | take a^b (mod c)
modExp :: (Integral a, Bits a) => a -> a -> a -> a
modExp a b c = expMod' a b c 1
  where
      expMod' a b c s
          | b == 0 = s
          | odd b = expMod' (a^2 `rem` c) (shift b (0 - 1)) c ((s * a) `mod` c)
          | otherwise = expMod' (a^2 `rem` c) (shift b (0 - 1)) c s

-- | Number of combinations of n choose r with factorial cancelling.
nCr :: Integral a => a -> a -> a
nCr n r = product [n,n - 1..n - r + 1] `div` factorial r

-- | Number of combinations of n choose r.
nCr' :: Integral a => a -> a -> a
n `nCr'` r = factorial n `div` (factorial r * factorial (n - r))

-- | Number of permutations of n pick r with factorial cancelling
nPr :: Integral a => a -> a -> a
nPr n r = product [n,n - 1..n - r + 1]

-- | Number of permutations of n pick r.
nPr' :: Integral a => a -> a -> a
n `nPr'` r = factorial n `div` factorial (n - r)

-- | Returns if n is an Integral to 7 decimal places.
isInt :: RealFrac a => a -> Bool
isInt n = (round $ 10^(fromIntegral 7) * (n - (fromIntegral $ round n))) == 0

-- | Returns True if n is a perfect square.
isSquare :: Integral a => a -> Bool
isSquare n = ((round (fromIntegral (n)**(0.5))) ^2 == n)

-- | Returns True if n is a perfect cube.
isCube :: Integral a => a -> Bool
isCube n = (round (fromIntegral (n)**(1 / 3))) ^3 == n

-- | The size of the list of numbers coprime to n.
eulerTotient :: Integer -> Integer
eulerTotient 1 = 1
eulerTotient n = product [(p - 1) * p ^ (c - 1) | (p, c) <- primeFactorsMult n]

-- | Euclid's Algorithm with forced strictness.
euclidGCD :: Integral a => a -> a -> a
euclidGCD (!a) (!b)
    | a `rem` b == 0 = b
    | otherwise = euclidGCD b (a `rem` b)
