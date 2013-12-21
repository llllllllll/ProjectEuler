-- |
-- Module      : ProjectEuler.Utils.Misc
-- Copyright   : Joe Jevnik 2013
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- Functions that do not fit any other ProjectEuler.Utils module.

module ProjectEuler.Utils.Misc
    ( isBouncy         -- :: (Integral a,Show a) => a -> Bool
    , isDecreasing     -- :: (Show a,Integral a) => a -> Bool
    , isIncreasing     -- :: (Show a,Integral a) => a -> Bool
    , isPandigitalr    -- :: (Integral a,Show a) => (Int,Int) -> a -> Bool
    , isPandigital     -- :: (Integral a,Show a) => (Int,Int) -> a -> Bool
    , isLychrel        -- :: (Integral a,Read a,Show a) => a -> Bool
    , binarySearch
    , bisectionSearch
    , bSearchInc
    , bSearchDec
    , aLength          -- :: (Num a, Ix a) => Array a e -> a
    , toRoman          -- :: Int -> String
    , fromRoman        -- :: String -> Int
    , showEngl         -- :: Int -> String
    ) where

import Data.Array              (Array,Ix,bounds,(!))
import Data.List               (sort,nub,groupBy)
import Data.String.Utils       (replace)
import ProjectEuler.Utils.List (intToList,listToInt)

-- | Returns a Bool whether a number is neither increasing or decreasing.
isBouncy :: (Integral a,Show a) => a -> Bool
isBouncy n = not (isIncreasing n || isDecreasing n)

-- |Returns a Bool as to whether a number is decreasing.
-- >>> isDecreasing 12344
-- False
-- >>> isDecreasing 44321
-- True.
isDecreasing :: (Show a,Integral a) => a -> Bool
isDecreasing n = case groupBy (>) $ intToList n of
                       [_] -> True
                       _   -> False

-- | Returns a Bool as to whether a number is increasing.
-- eg: isIncreasing 12344 = True, isIncreasing 44321 = False.
isIncreasing :: (Show a,Integral a) => a -> Bool
isIncreasing n = case groupBy (<) $ intToList n of
                     [_] -> True
                     _   -> False

-- | Returns if n is pandigital allowing repeats with bounds a..b.
isPandigitalr :: (Integral a,Show a) => (Int,Int) -> a -> Bool
isPandigitalr (a,b) n = (nub . sort . intToList) n == [a..b]

-- | Return if n is pandigital NOT allowing repeats with bounds a..b.
isPandigital :: (Integral a,Show a) => (Int,Int) -> a -> Bool
isPandigital (a,b) n = (sort . intToList) n == [a..b]

-- | Detirmines if a number is a lychrel number,
-- a number that does not converge to a palindrome.
isLychrel :: (Integral a,Read a,Show a) => a -> Bool
isLychrel n = isLychrel' n 0
  where
      isLychrel' n c
          | (reverse . show) n == show n && c /= 0 = False
          | c == 50 = True
          | otherwise =
                isLychrel' (listToInt ((reverse . intToList) n) + n) (c + 1)

-- | A Binary Search for arrays
binarySearch n arr = binarySearch' n arr 0 (aLength arr)
binarySearch' n arr a b
    | arr ! c == n = c
    | arr ! c > n = binarySearch' n arr a (c-1)
    | arr ! c < n = binarySearch' n arr (c+1) b
    | otherwise = 0-1
  where c = floor (fromIntegral (a + b) / 2)

-- | Simple Bisection search taking a function f, a value to search for n,
-- a tuple indicating range, and an epsilon or accuracy level.e
bisectionSearch f n (a,b) eps
    | f a > f b = bSearchDec f n (a,b) eps
    | otherwise = bSearchInc f n (a,b) eps

-- | Bisection search for a function that is increasing over the range (a,b).
bSearchInc f n (a,b) eps
    | f c == n || (b - a) / 2 < eps = c
    | f c > n = bSearchInc f n (a,c) eps
    | otherwise = bSearchInc f n (c,b) eps
  where c = (a + b) / 2.0

-- | Bisection search for a function that is decreasing over the range (a,b).
bSearchDec f n (a,b) eps
    | f c == n || (b - a) / 2 < eps = c
    | f c < n = bSearchDec f n (a,c) eps
    | otherwise = bSearchDec f n (c,b) eps
  where c = (a + b) / 2.0

-- | The length of array arr.
aLength :: (Num a, Ix a) => Array a e -> a
aLength arr = (snd . bounds) arr - (fst . bounds) arr

-- | All possible roman numeral tokens.
romanNumerals :: [(Int,String)]
romanNumerals = [(1000,"M"),(900,"CM"),(500,"D"),(400,"CD"),(100,"C"),(90,"XC")
                 ,(50,"L"),(40,"XL"),(10,"X"),(9,"IX"),(5,"V"),(4,"IV"),(1,"I")]

-- | Converts a number into its most efficent roman numberal representation.
toRoman :: Int -> String
toRoman 0 = "N"
toRoman x = toRoman' x
  where
      toRoman' x
	  | x == 0 = ""
	  | x > 0 = b ++ toRoman' (x - a)
	    where (a, b) = head $ filter ((<= x) . fst) romanNumerals

-- | Parses a string representation of roman numerals into there
-- arabic numeral equivalent.
fromRoman :: String -> Int
fromRoman str = sum $ map f
                 $ replace "CM" "m"
                 $ replace "CD" "d"
                 $ replace "XC" "c"
                 $ replace "XL" "l"
                 $ replace "IX" "x"
                 $ replace "IV" "v" str
  where
      f c
          | c == 'M' = 1000
          | c == 'm' = 900
          | c == 'D' = 500
          | c == 'd' = 400
          | c == 'C' = 100
          | c == 'c' = 90
          | c == 'L' = 50
          | c == 'l' = 40
          | c == 'X' = 10
          | c == 'x' = 9
          | c == 'V' = 5
          | c == 'v' = 4
          | c == 'I' = 1

-- | Converts an Int to its english representation.
showEngl :: Int -> String
showEngl n
    | n < 0 = "negative " ++ showEngl (-n)
    | n < 20 = us!!n
    | n < 100 = ts!!(n `div` 10) ++ (if n `rem` 10 /= 0
                                   then " "
                                   else "") ++ us!!(n `rem` 10)
    | n < 1000 = us!!(n `div` 100) ++ " hundred" ++ (if n `rem` 100 /= 0
                                                       then " and"
                                                       else "")
                 ++ showEngl (n `rem` 100)
    | n < 1000000 = showEngl (n `div` 1000) ++ " thousand" ++
                    (if n `rem` 1000 /= 0
                       then " "
                       else "") ++ showEngl (n `rem` 1000)
    | n < 1000000000 = showEngl (n `div` 100000) ++ " million" ++
                       (if n `rem` 1000000 /= 0
                          then " "
                          else "") ++ showEngl (n `rem` 1000000)
    | otherwise = showEngl (n `div` 1000000000) ++ " billion" ++
                  (if n `rem` 1000000000 /= 0
                     then " "
                     else "") ++ showEngl (n `rem` 1000000000)
  where
      us :: [String]
      us = ["", "one", "two", "three", "four", "five", "six", "seven"
           , "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen"
           , "fifteen", "sixteen", "seventeen", "eighteen", "nineteen" ]

      ts :: [String]
      ts = [ "",""
           , "twenty"
           , "thirty"
           , "forty"
           , "fifty"
           , "sixty"
           , "seventy"
           , "eighty"
           , "ninety"
           ]
