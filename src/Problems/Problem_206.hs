-- NOT COMPLETED.
module Problems.Problem_206
    ( problem_206
    ) where

problem_206 = [n | n <- [1020304050607080900..1929394959697989990], 
               is_valid (show n) 0]
    where
	find_x x
            | is_valid (show (x^2)) 0 = x
            | otherwise = find_x (x+1)
	is_valid (n:ns) c
            | null (n:ns) = True
            | c == 0 && n == '1' = is_valid ns (c+1)
            | c == 2 && n == '2' = is_valid ns (c+1)
            | c == 4 && n == '3' = is_valid ns (c+1)
            | c == 6 && n == '4' = is_valid ns (c+1)
            | c == 8 && n == '5' = is_valid ns (c+1)
            | c == 10 && n == '6' = is_valid ns (c+1)
            | c == 12 && n == '7' = is_valid ns (c+1)
            | c == 14 && n == '8' = is_valid ns (c+1)
            | c == 16 && n == '9' = is_valid ns (c+1)
            | c == 18 && n == '0' = is_valid ns (c+1)
            | odd c = is_valid ns (c+1)
            | otherwise = False