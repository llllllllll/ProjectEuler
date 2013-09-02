-- NOT YET COMPLETED.
module Problems.Problem_26
    ( problem_26
    , recip_len
    ) where

import Data.List

problem_26 = 0

recip_len :: Int -> Int
recip_len d = recip_len' (drop 2 $ show (1 `div` d)) [] 0
  where
      recip_len' _ _ _ = 0
