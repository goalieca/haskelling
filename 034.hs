--project euler problem 34
{--
145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

Find the sum of all numbers which are equal to the sum of the factorial of their digits.

Note: as 1! = 1 and 2! = 2 are not sums they are not included.
--}

import Char

--precalculated for performance
fac 0 = 1
fac 1 = 1
fac 2 = 2
fac 3 = 6
fac 4 = 24
fac 5 = 120
fac 6 = 720
fac 7 = 5040
fac 8 = 40320
fac 9 = 362880

digits = map (`mod` 10) . takeWhile (> 0) . iterate (`div` 10)

fac_sum_digits = sum . map fac . digits

valid x = x == (fac_sum_digits x)

-- seems to be a suitable upper bound
maxNum = 10 * fac 9

main = do
    let filtered = filter valid [3..maxNum]
    print $ sum filtered
