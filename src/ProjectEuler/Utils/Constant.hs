-- |
-- Module      : ProjectEuler.Utils.Constant
-- Copyright   : Joe Jevnik 2013
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- Utilities representing constans.

module ProjectEuler.Utils.Constant
    ( e            -- :: Floating a => a
    , champernowne -- :: String
    ) where

-- | Euler's number.
e :: Floating a => a
e = exp 1

-- | Champernown's constant.
champernowne :: String
champernowne = concatMap show [0..]
