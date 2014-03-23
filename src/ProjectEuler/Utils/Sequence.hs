-- |
-- Module      : ProjectEuler.Utils.Sequence
-- Copyright   : Joe Jevnik 2013
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- Functions dealing with sequences of numbers.

module ProjectEuler.Utils.Sequence
    ( collatz        -- :: Integral a => a -> [a]
    , collatzMem     -- :: Int -> [Int]
    , fib            -- :: Int -> Integer
    , fibs           -- :: [Integer]
    , slowFib        -- :: Int -> Int
    , fareySeqTerm   -- :: Integral a => Ratio a -> Ratio a -> a -> Ratio a
    , fareySeqLength -- :: Integer -> Integer
    ) where

import Data.List                 (foldl',mapAccumR)
import Data.Bits                 (testBit,bitSize)
import Data.Ratio                (Ratio,(%),denominator,numerator)
import ProjectEuler.Utils.Number (eulerTotient)

-- | Collatz Sequence starting at n.
collatz :: Integral a => a -> [a]
collatz 1 = []
collatz n
    | even n = n:collatz (n `div` 2)
    | odd n  = n:collatz (n * 3 + 1)

-- | Memoized Collatz Sequence starting at n.
collatzMem :: Int -> [Int]
collatzMem = ((map c [0..])!!)
    where
	c 1 = [1]
	c n
            | even n = n : collatzMem (n `div` 2)
            | odd n  = n : collatzMem (n * 3 + 1)

-- | The nth fibonacci number.
-- Credit to: http://www.haskell.org/haskellwiki/The_Fibonacci_sequence
fib :: Int -> Integer
fib n = snd . foldl' fib' (1, 0) . dropWhile not
        $ [testBit n k | k <- let s = bitSize n in [s - 1,s - 2..0]]
    where
        fib' (f, g) p
            | p         = (f * (f + 2 * g),ss)
            | otherwise = (ss,g * (2 * f - g))
            where
                ss = f * f + g * g

-- | An infinite list of fibonnaci numbers.
fibs :: [Integer]
fibs = scanl (+) 0 (1:fibs)

-- | A Fibbonacci term without memoization.
slowFib :: Int -> Int
slowFib 0 = 0
slowFib 1 = 1
slowFib n = slowFib (n - 2) + slowFib (n - 1)

-- | Farey Sequence term to the left of b with a power of p.
fareySeqTerm :: Integral a => Ratio a -> Ratio a -> a -> Ratio a
fareySeqTerm a b p
    | da2 <= p = fareySeqTerm a1 b p
    | otherwise = na % nb
  where
      na  = numerator a
      nb  = numerator b
      da  = denominator a
      db  = denominator b
      a1  = (na + nb) % (da + db)
      da2 = denominator a1

-- | The length of the farey Sequence of order n.
fareySeqLength :: Integer -> Integer
fareySeqLength n = foldr ((+) . eulerTotient) 1 [2..n]
