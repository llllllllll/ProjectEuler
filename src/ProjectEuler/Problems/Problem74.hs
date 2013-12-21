-- 402 - Completed 17.5.2013 - Learned about Factorial Chains.
module ProjectEuler.Problems.Problem74
    ( problem74 -- IO ()
    ) where

import ProjectEuler.Utils.List   (intToList)
import ProjectEuler.Utils.Number (factorial)

problem74 :: IO ()
problem74 = print $ length [fChain n| n <- [1..999999], isValid n]
    where
	isValid n = fChain n == 60
	fChain n = fChain' n []
	fChain' n ns
            | n `elem` ns = length ns
	    | otherwise = fChain' ((sum . map factorial)
                                   (intToList n)) (n:ns)
