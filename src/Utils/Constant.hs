module Utils.Constant
    ( e
    , champernowne
    ) where

-- Euler's number.
e = exp 1

-- Champernown's constant.
champernowne :: String
champernowne = concatMap show [0..]
