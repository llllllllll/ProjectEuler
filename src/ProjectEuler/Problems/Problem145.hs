-- 608720 - Completed 26.5.2013 - Horrible solution must fix.
module ProjectEuler.Problems.Problem145
    ( problem145
    ) where

import Data.Char (digitToInt)
import ProjectEuler.Utils.List (intToList)

problem145 = print $ 2 * length [(n, rev n) | n <- [1..10^9],
                         let l = intToList n
                         in (even . head) l && (odd . last) l
                                 && isValid ((show . rev) n) n]
  where
      rev n
          | n `rem` 10 == 0 = 2
          | otherwise = n + (read . reverse . show) n :: Integer
      isValid (n:ns) x
          | null ns && (odd . digitToInt) n = True
          | (odd . digitToInt) n = isValid ns x
          | (even . digitToInt) n = False
