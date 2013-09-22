-- 16695334890 - Completed 21.5.2013.
module Problems.Problem_43
    ( problem_43
    ) where

import Data.List (permutations)

problem_43 = sum $ (map (read) [x | x <- permutations ['0'..'9'], 
                                is_valid x] :: [Integer])
    where
	is_valid x = (even . read) [x!!1,x!!2,x!!3] 
                     && read [x!!2,x!!3,x!!4] `rem` 3 == 0 
                     && read [x!!3,x!!4,x!!5] `rem` 5 == 0
		     && read [x!!4,x!!5,x!!6] `rem` 7 == 0 
                     && read [x!!5,x!!6,x!!7] `rem` 11 == 0 
                     && read [x!!6,x!!7,x!!8] `rem` 13 == 0
		     && read [x!!7,x!!8,x!!9] `rem` 17 == 0 
