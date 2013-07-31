-- 210 - Completed 15.5.2013 - Leaned about Champernowne's Constant.
module Problems.Problem_40
    ( problem_40
    ) where

import Data.Char
import Utils.Constant

problem_40 = product $ map (digitToInt . (!!) champernowne) indecies
    where
	indecies = map (10^) [1..6]