import Utils.Misc
import Utils.Number

f n = (12^n - (factorial 12 `div` factorial (12 - n))) `div` 12^n

search = bisection_search f 0.5 (2,12) 0.00001
