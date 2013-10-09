-- NOT COMPLETED.
module Problems.Problem_142
    ( problem_142
    ) where

import Data.List (foldl)
import Utils.Number (is_square)

problem_142 = head [x+y+z | x <- [1..1000], y <- [1..(x-1)], z <- [1..(y-1)], 
                    is_valid x y z]
    where
	is_valid x y z = foldl (&&) True (map is_square 
                                                  [(x+y),(x-y),(x+z),(x-z),
                                                   (y+z),(y-z)])
