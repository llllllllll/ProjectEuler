module Utils.Vector 
    ( Vector(..)
    , (|+|)
    , (|-|)
    , (|.|)
    , v_negate
    , cross_prod
    , dot_prod
    , magnitude
    , angle_between
    , distance
    ) where

type Vector = (Double,Double)

-- Adds two Vectors
(|+|) :: Vector -> Vector -> Vector
(|+|) (v1,v2) (u1,u2) = (v1 + u1,v2 + u2)

-- Subtracs two Vectors
(|-|) :: Vector -> Vector -> Vector
(|-|) (v1,v2) (u1,u2) = (v1 - u1,v2 - u2)

-- Flips the sign of (a,b)
v_negate (a,b) = (0-a,0-b)

-- a X b
cross_prod :: Vector -> Vector -> Vector
cross_prod a b = ((magnitude a * magnitude b * cos (angle_between a b)),
                  (magnitude a * magnitude b * sin (angle_between a b)))

-- a * b
dot_prod :: Vector -> Vector -> Double
dot_prod (a,b) (c,d) = a*c + b*d

-- Alias for dot_prod a b
(|.|) a b = dot_prod a b

-- "||<a,b>||"
magnitude :: Vector -> Double
magnitude (a,b) = sqrt (a^2 + b^2)

-- The angle in radians between vector a and vector b.
angle_between :: Vector -> Vector -> Double
angle_between a b = acos $ (a |.| b) / (magnitude b / magnitude a)

-- Euclidian distance between point a and point b.
distance :: Vector -> Vector -> Double
distance a b = sqrt ((fst a - fst b)^2 + (snd a - snd b)^2)


