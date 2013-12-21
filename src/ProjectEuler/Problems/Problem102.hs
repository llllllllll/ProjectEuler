-- 228 - Completed 1.9.2013
module ProjectEuler.Problems.Problem102
    ( problem102 -- :: IO ()
    , inTriangle -- :: Vector -> (Vector,Vector,Vector) -> Bool
    ) where

import Control.Applicative       ((<$>))
import Data.Array                (listArray,(!))
import ProjectEuler.Utils.Vector (Vector(..),(|-|),(|.|))
import ProjectEuler.Utils.List   (splitOn)

type Triangle = (Vector,Vector,Vector)

problem102 :: IO ()
problem102 = length . filter (inTriangle (0,0)) <$> parseTriangles >>= print

inTriangle :: Vector -> Triangle -> Bool
inTriangle p (a,b,c) = let v0    = c |-| a
                           v1    = b |-| a
                           v2    = p |-| a
                           dot00 = v0 |.| v0
                           dot01 = v0 |.| v1
                           dot02 = v0 |.| v2
                           dot11 = v1 |.| v1
                           dot12 = v1 |.| v2
                           invD = 1 / (dot00 * dot11 - dot01 * dot01)
                           u     = (dot11 * dot02 - dot01 * dot12) * invD
                           v     = (dot00 * dot12 - dot01 * dot02) * invD
                       in u >= 0 && v >= 0 && u + v < 1

parseTriangles =
    map (mkTri . splitOn (==',')) . lines <$> readFile "txt/triangles.txt"
  where
      mkTri ps = let p = listArray (0,5) (map read ps)
                  in ((p!0,p!1),(p!2,p!3),(p!4,p!5))
