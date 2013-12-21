-- 872187 - Completed 3.6.2013 - Learned ShowS.
module ProjectEuler.Problems.Problem36
    ( problem36 -- :: IO ()
    ) where

import Data.Char (intToDigit)
import Numeric (showIntAtBase)

problem36 :: IO ()
problem36 = print $ sum [n | n <- [1..999999]
                        , show n == (reverse . show) n && (showIntAtBase 2
                                                           intToDigit n) ""
                                   == reverse ((showIntAtBase 2
                                                intToDigit n) "")]
