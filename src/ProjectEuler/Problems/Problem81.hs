-- NOT YET COMPLETED.
module Problems.Problem81
    ( problem81
   -- , parseGrid
    ) where

import Control.Applicative ((<$>))
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe
import Data.Array.Unboxed
import Utils.List (splitOn)

problem81 = error "not complete"

{-
problem81 :: IO (Maybe [(Int,Int)])
problem81 = do
    --g <- parseGrid
    let g = testGrid
    return $ (search (0,0) (79,79) [] g) 

search :: Int -> UArray (Int,Int) Int
costMatrix r g = g//[i,if g!(r,c)

parseGrid :: IO (UArray (Int,Int) Int)
parseGrid = (map (map read)) . (map (splitOn (==','))) . lines
              <$> readFile "txt/matrix.txt" 
              >>= (\vs -> return $ listArray ((0,0),(79,79)) (concat vs))

testGrid :: UArray (Int,Int) Int
testGrid = let g = [ [ 0 , 1, 2 ]
                    , [ 0 , 1, 2 ]
                    , [ 0 , 0, 1 ] ]
            in listArray ((0,0),(2,2)) (concat g)
-}
 