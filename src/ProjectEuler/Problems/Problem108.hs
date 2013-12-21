-- NOT COMPLETED.
module Problems.Problem108
    ( problem108
    ) where

import Data.Ratio

problem108 = map (length . solutions) [1..]
    where
	solutions n = [(x,y) | x <- [1..500], y <- [1..x], 
                                    1%x+1%y==1%n] 