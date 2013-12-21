
module Problems.Problem44
    ( problem44
    ) where

import Utils.Number (isInt)

problem44 = [pentNum a - pentNum b 
             | a <- [1..5000], b <- [1..a], 
               isPent (pentNum a - pentNum b) 
               && isPent (pentNum a + pentNum b)]
    where
	isPent n = isInt ((1/6)*(1-sqrt (fromIntegral (24*n+1)))) 
                    || isInt ((1/6)*(1+sqrt (fromIntegral (24*n+1))))
	pentNum = ((map (\n -> n*(3*n-1)`div`2) [1..])!!)
 