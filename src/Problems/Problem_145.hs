-- 608720 - Completed 26.5.2013 - Horrible solution must fix.
module Problems.Problem_145
    ( problem_145
    ) where

import Data.Char
import Utils.List

problem_145 = 2* length [(n, rev n) | n <- [1..10^9], 
                         let l = int_to_list n 
                         in (even . head) l && (odd . last) l 
                                && is_valid ((show . rev) n) n]
    where
	rev n 
            | n `rem` 10 == 0 = 2
            | otherwise = n + (read . reverse . show) n :: Integer
	is_valid (n:ns) x
            | null ns && (odd . digitToInt) n = True
            | (odd . digitToInt) n = is_valid ns x
            | (even . digitToInt) n = False