-- |
-- Module      : ProjectEuler.Utils.Vector
-- Copyright   : Joe Jevnik 2013
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- Functions dealing with 2 dimension vectors.

module ProjectEuler.Utils.Vector
    ( Vector(..)
    , (|+|)        -- :: Vector -> Vector -> Vector
    , (|-|)        -- :: Vector -> Vector -> Vector
    , (|.|)        -- :: Vector -> Vector -> Double
    , vNegate      -- :: Vector -> Vector
    , crossProd    -- :: Vector -> Vector -> Vector
    , dotProd      -- :: Vector -> Vector -> Double
    , magnitude    -- :: Vector -> Double
    , angleBetween -- :: Vector -> Vector -> Double
    , distance     -- :: Vector -> Vector -> Double
    ) where

type Vector = (Double,Double)

-- | Adds two Vectors
(|+|) :: Vector -> Vector -> Vector
(|+|) (v1,v2) (u1,u2) = (v1 + u1,v2 + u2)

-- | Subtracs two Vectors
(|-|) :: Vector -> Vector -> Vector
(|-|) (v1,v2) (u1,u2) = (v1 - u1,v2 - u2)

-- | Flips the sign of (a,b)
vNegate :: Vector -> Vector
vNegate (a,b) = (-a,-b)

-- | a X b
crossProd :: Vector -> Vector -> Vector
crossProd a b = ( magnitude a * magnitude b * cos (angleBetween a b)
                , magnitude a * magnitude b * sin (angleBetween a b) )

-- | a * b
dotProd :: Vector -> Vector -> Double
dotProd (a,b) (c,d) = a * c + b * d

-- | Alias for dotProd a b
(|.|) :: Vector -> Vector -> Double
(|.|) = dotProd

-- | "||<a,b>||"
magnitude :: Vector -> Double
magnitude (a,b) = sqrt (a^2 + b^2)

-- | The angle in radians between vector a and vector b.
angleBetween :: Vector -> Vector -> Double
angleBetween a b = acos $ (a |.| b) / (magnitude b / magnitude a)

-- | Euclidian distance between point a and point b.
distance :: Vector -> Vector -> Double
distance a b = sqrt $ (fst a - fst b)^2 + (snd a - snd b)^2
