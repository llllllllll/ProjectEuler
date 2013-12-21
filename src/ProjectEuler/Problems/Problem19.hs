-- NOT YET COMPLETED.
module ProjectEuler.Problems.Problem19
    ( problem19  -- IO ()
    , isLeapYear -- Int -> Boolx
    ) where

months :: [(String,Int)]
months = [ ("Jan",31)
         , ("Feb",28)
         , ("Mar",31)
         , ("Apr",30)
         , ("May",31)
         , ("Jun",30)
         , ("Jul",31)
         , ("Aug",31)
         , ("Sep",30)
         , ("Oct",31)
         , ("Nov",30)
         , ("Dec",31)
         ]

isLeapYear :: Int -> Bool
isLeapYear y = y `rem` 4 == 0 && (not (y `rem` 100 == 0) || y `rem` 400 == 0)

problem19 = return ()
