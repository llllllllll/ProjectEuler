import System.IO

main :: IO ()
main = do
    files <- readFile "count"
    file_cont <- mapM readFile (lines files)
    putStr $ concat file_cont
