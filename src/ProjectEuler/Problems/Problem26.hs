-- NOT YET COMPLETED.
module Problems.Problem26
    ( problem26
    , recipLen
    ) where

import Data.List

problem26 = 0

recipLen :: Int -> Int
recipLen d = recipLen' (drop 2 $ show (1 `div` d)) [] 0
  where
      recipLen' _ _ _ = 0
