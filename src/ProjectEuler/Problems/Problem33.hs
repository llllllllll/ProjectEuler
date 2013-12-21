-- 100 - Completed 23.5.2013.
module ProjectEuler.Problems.Problem33
    ( problem33 -- :: IO ()
    ) where

import Data.Ratio              ((%),denominator)
import Data.List               (delete,intersect)
import ProjectEuler.Utils.List (listToInt,intToList)

problem33 :: IO ()
problem33 = print $ denominator
            $ product  [ a % b | a <- [10..99]
                       , b <- [10..99]
                       , a % b < 1 && a /= b && a `rem` 10 /= 0
                               && b `rem` 10 /= 0  && isValid a b]
    where
	isValid a b = (not . null) (intersect (intToList a) (intToList b))
                      && let s = head (intersect (intToList a) (intToList b))
                         in (listToInt (delete s (intToList a))) %
                                (listToInt (delete s (intToList b))) == a % b
