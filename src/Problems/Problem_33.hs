-- 100 - Completed 23.5.2013.
module Problems.Problem_33
    ( problem_33
    ) where

import Data.Ratio
import Data.List
import Utils.List

problem_33 = denominator $ product 
             [a%b | a <- [10..99], b <- [10..99], 
              a%b < 1 && a /= b && a `rem` 10 /= 0 && b `rem` 10 /= 0 
              && is_valid a b]
    where
	is_valid a b = 
            (not . null) (intersect (int_to_list a) (int_to_list b)) && 
            let s = head (intersect (int_to_list a) (int_to_list b)) 
            in (list_to_int (delete s (int_to_list a))) % 
                   (list_to_int (delete s (int_to_list b))) == a%b