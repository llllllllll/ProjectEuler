-- 872187 - Completed 3.6.2013 - Learned ShowS.
module Problems.Problem_36
    ( problem_36
    ) where

import Data.Char
import Numeric

problem_36 = sum [n | n <- [1..999999], 
                  show n == (reverse . show) n 
                  && (showIntAtBase 2 intToDigit n) "" 
                  == reverse ((showIntAtBase 2 intToDigit n) "")]