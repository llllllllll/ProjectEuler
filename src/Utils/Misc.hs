module Utils.Misc 
    ( divisors
    , r_divisors
    , factorial
    , num_divisors
    , is_bouncy
    , is_decreasing
    , is_increasing
    , is_pandigitalr
    , is_pandigital
    , nCr
    , nPr
    , is_int
    , is_square
    , is_cube
    , euler_totient
    , is_lychrel
    , binary_search
    , bisection_search
    , b_search_inc
    , b_search_dec
    , a_length
    , partitions
    , exp_by_sq
    , hyper_exp
    , mod_exp
    , to_roman
    , from_roman
    , show_engl
    ) where

import Data.List (genericLength,nub,(\\),sort)
import Data.Array (bounds,(!))
import Data.Bits (Bits(..),shift)
import Data.String.Utils (replace)
import Utils.List (int_to_list,list_to_int)
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
factorial n = product [1..n]

-- The number of divisors of n.
num_divisors :: Integral a => a -> Integer
num_divisors n = (genericLength . divisors) n

-- Returns a Bool whether a number is neither increasing or decreasing.
is_bouncy n = not (is_increasing n || is_decreasing n)

-- Returns a Bool as to whether a number is decreasing. 
-- eg: is_decreasing 12344 = False, is_decreasing 44321 = True.
is_decreasing n = is_increasing' (head (show n)) (tail (show n))
  where
      is_increasing' n (x:xs)
	  | null (x:xs) = False
	  | null xs  && x <= n = True
	  | x <= n = is_increasing' x xs
	  | otherwise = False

-- Returns a Bool as to whether a number is increasing. 
-- eg: is_increasing 12344 = True, is_increasing 44321 = False.
is_increasing n = is_decreasing' (head (show n)) (tail (show n))
  where
      is_decreasing' n (x:xs)
	  | null (x:xs) = False
	  | null xs && x >= n = True
	  | x >= n = is_decreasing' x xs
	  | otherwise = False

-- Returns if n is pandigital allowing repeats with bounds a..b.
is_pandigitalr (a,b) n = (nub . sort . int_to_list) n == [a..b]

-- Return if n is pandigital NOT allowing repeats with bounds a..b.
is_pandigital (a,b) n = (length . int_to_list) n == b && is_pandigital' [a..b]
                        (int_to_list n) []
  where
      is_pandigital' ab (n:ns) vals
	  | n `elem` ab && not (n `elem` vals) && null ns = True
	  | n `elem` ab && not ( n `elem` vals) = is_pandigital'
                                                    ab ns (n:vals)
	  | otherwise = False

-- Number of combinations of n choose r.
n `nCr` r = factorial n / (factorial r * factorial (n-r))

-- Number of permutation of n pick r.
n `nPr` r = factorial n / factorial (n-r)

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
euler_totient m = product 
                  [(p - 1) * p ^ (c - 1) | (p, c) <- prime_factors_mult m]

-- Detirmines if a number is a lychrel number,
-- a number that does not converge to a palindrome.
is_lychrel n = is_lychrel' n 0
  where
      is_lychrel' n c
          | (reverse . show) n == show n && c /= 0 = False
          | c == 50 = True
          | otherwise = 
                is_lychrel' (list_to_int ((reverse . int_to_list) n) + n) (c+1)

-- A Binary Search for arrays
binary_search n arr = binary_search' n arr 0 (a_length arr)
binary_search' n arr a b
    | arr ! c == n = c
    | arr ! c > n = binary_search' n arr a (c-1)
    | arr ! c < n = binary_search' n arr (c+1) b
    | otherwise = 0-1
  where c = floor (fromIntegral (a + b) / 2)

-- Simple Bisection search taking a function f, a value to search for n, 
-- a tuple indicating range, and an epsilon or accuracy level.e
bisection_search f n (a,b) eps
    | f a > f b = b_search_dec f n (a,b) eps
    | otherwise = b_search_inc f n (a,b) eps

-- Bisection search for a function that is increasing over the range (a,b).
b_search_inc f n (a,b) eps
    | f c == n || (b - a) / 2 < eps = c
    | f c > n = b_search_inc f n (a,c) eps
    | otherwise = b_search_inc f n (c,b) eps
  where c = (a + b) / 2.0

-- Bisection search for a function that is decreasing over the range (a,b).
b_search_dec f n (a,b) eps
    | f c == n || (b - a) / 2 < eps = c
    | f c < n = b_search_dec f n (a,c) eps
    | otherwise = b_search_dec f n (c,b) eps
  where c = (a + b) / 2.0

-- The length of array arr.
a_length arr = (snd . bounds) arr - (fst . bounds) arr

-- The number of partitions in n.
partitions n = c n n
  where
      c 0 _ = 1
      c _ 0 = 1
      c n m = sum $ map (\x -> l!!(n-x)!!min (n-x) x) [1..m]
      l = [[c n m | m <- [0..n]] | n<-[0..]]
                  
-- Exponentiation by squares in O(log b).
exp_by_sq :: Num a => a -> Integer -> a
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
          | b == 0    = s
          | odd b     = exp_mod' (a^2 `rem` c) (shift b (0-1)) c ((s * a) `mod` c)
          | otherwise = exp_mod' (a^2 `rem` c) (shift b (0-1)) c s
                  
roman_numerals :: [(Int,String)]
roman_numerals = [(1000,"M"),(900,"CM"),(500,"D"),(400,"CD"),(100,"C"),(90,"XC")
                 ,(50,"L"),(40,"XL"),(10,"X"),(9,"IX"),(5,"V"),(4,"IV"),(1,"I")]

-- Converts a number into its most efficent roman numberal representation.
to_roman :: Int -> String
to_roman 0 = "N"
to_roman x = to_roman' x
  where
      to_roman' x 
	  | x == 0 = ""
	  | x > 0 = b ++ to_roman' (x - a)
	    where (a, b) = head $ filter ((<= x) . fst) roman_numerals
                           
-- Parses a string representation of roman numerals into there 
-- arabic numeral equivalent.
from_roman :: String -> Int
from_roman str = sum $ map f 
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

-- Converts an Int to its english representation.
show_engl :: Int -> String
show_engl n
    | n < 0 = "negative " ++ show_engl (-n)
    | n < 20 = us!!n
    | n < 100 = ts!!(n `div` 10) ++ (if n `rem` 10 /= 0
                                   then " "
                                   else "") ++ us!!(n `rem` 10)
    | n < 1000 = us!!(n `div` 100) ++ " hundred" ++ (if n `rem` 100 /= 0
                                                       then " and"
                                                       else "")
                 ++ show_engl (n `rem` 100)
    | n < 1000000 = show_engl (n `div` 1000) ++ " thousand" ++ 
                    (if n `rem` 1000 /= 0
                       then " "
                       else "") ++ show_engl (n `rem` 1000)
    | n < 1000000000 = show_engl (n `div` 100000) ++ " million" ++
                       (if n `rem` 1000000 /= 0
                          then " "
                          else "") ++ show_engl (n `rem` 1000000)
    | otherwise = show_engl (n `div` 1000000000) ++ " billion" ++
                  (if n `rem` 1000000000 /= 0
                     then " "
                     else "") ++ show_engl (n `rem` 1000000000)
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
