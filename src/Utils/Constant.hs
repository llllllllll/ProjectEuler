module Utils.Constant 
    ( e
    , champernowne
    ) where

import Data.List

-- Euler's number.
e = exp 1

-- Champernown's constant.
champernowne :: String
champernowne = concatMap show [0..]