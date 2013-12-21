-- NOT COMPLETED.
module Problems.Problem142
    ( problem142
    ) where

import Data.List (foldl)
import Utils.Number (isSquare)

problem142 = head [x+y+z | x <- [1..1000], y <- [1..(x-1)], z <- [1..(y-1)], 
                    isValid x y z]
    where
	isValid x y z = foldl (&&) True (map isSquare 
                                                  [(x+y),(x-y),(x+z),(x-z),
                                                   (y+z),(y-z)])
 