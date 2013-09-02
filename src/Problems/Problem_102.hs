-- 228 - Completed 1.9.2013
module Problems.Problem_102
    ( problem_102
    , in_triangle
    ) where

import Control.Applicative
import Data.Array
import Utils.Vector
import Utils.List

type Triangle = (Vector,Vector,Vector)

problem_102 = length . filter (in_triangle (0,0)) <$> parse_triangles

in_triangle :: Vector -> Triangle -> Bool
in_triangle p (a,b,c) = let v0    = c |-| a
                            v1    = b |-| a
                            v2    = p |-| a
                            dot00 = v0 |.| v0
                            dot01 = v0 |.| v1
                            dot02 = v0 |.| v2
                            dot11 = v1 |.| v1
                            dot12 = v1 |.| v2
                            inv_d = 1 / (dot00 * dot11 - dot01 * dot01)
                            u     = (dot11 * dot02 - dot01 * dot12) * inv_d
                            v     = (dot00 * dot12 - dot01 * dot02) * inv_d
                        in u >= 0 && v >= 0 && u + v < 1

parse_triangles = 
    map (mk_tri . split_on (==',')) . lines <$> readFile "txt/triangles.txt"
  where 
      mk_tri ps = let p = listArray (0,5) (map read ps) 
                  in ((p!0,p!1),(p!2,p!3),(p!4,p!5))

