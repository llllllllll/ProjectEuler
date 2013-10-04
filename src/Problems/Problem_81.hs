-- NOT YET COMPLETED.
module Problems.Problem_81
    ( problem_81
   -- , parse_grid
    ) where

import Control.Applicative ((<$>))
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe
import Data.Array.Unboxed
import Utils.List (split_on)

problem_81 = error "not complete"

{-
problem_81 :: IO (Maybe [(Int,Int)])
problem_81 = do
    --g <- parse_grid
    let g = test_grid
    return $ (search (0,0) (79,79) [] g) 

search :: Int -> UArray (Int,Int) Int
cost_matrix r g = g//[i,if g!(r,c)

parse_grid :: IO (UArray (Int,Int) Int)
parse_grid = (map (map read)) . (map (split_on (==','))) . lines
              <$> readFile "txt/matrix.txt" 
              >>= (\vs -> return $ listArray ((0,0),(79,79)) (concat vs))

test_grid :: UArray (Int,Int) Int
test_grid = let g = [ [ 0 , 1, 2 ]
                    , [ 0 , 1, 2 ]
                    , [ 0 , 0, 1 ] ]
            in listArray ((0,0),(2,2)) (concat g)
-}
