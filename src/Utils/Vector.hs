module Utils.Vector 
    ( Vector(..)
     cross_prod
    , dot_prod
    , magnitude
    , angle_between
    , distance
    ) where

data Vector = Floating a => (a,a)

-- a X b
cross_prod :: Vector -> Vector -> Vector
cross_prod a b = ((magnitude a * magnitude b * cos (angle_between a b)),
                  (magnitude a * magnitude b * sin (angle_between a b)))

-- a * b
dot_prod :: Vector -> Vector -> Double
dot_prod (a,b) (c,d) = a*c + b*d

-- "||<a,b>||"
magnitude :: Vector -> Double
magnitude (a,b) = sqrt (a^2 + b^2)

-- The angle in radians between vector a and vector b.
angle_between :: Vector -> Vector -> Double
angle_between a b = asin (magnitude b / magnitude a)

-- Euclidian distance between point a and point b.
distance :: Vector -> Vector -> Double
distance a b = sqrt ((fst a - fst b)^2 + (snd a - snd b)^2)