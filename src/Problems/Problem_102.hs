-- NOT COMPLETED.
module Problems.Problem_102
    ( 
    ) where

import System.IO

{-problem_102 = do
	file <- readFile "txt/triangles.txt"
	let
		triangles = [(\xs -> 
                               ((read (xs!!0)::Double,read (xs!!1)::Double),
                                (read (xs!!2)::Double,read (xs!!3)::Double),
                                (read (xs!!4)::Double,read (xs!!5)::Double))) 
                             $ split_on (==',') str | str <- lines file]
		origin = (0,0)
	print $ length $ filter contains_origin triangles-}