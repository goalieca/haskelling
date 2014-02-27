--problem #1
--project euler
{--
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
--}

isMult num = (num `mod` 3 == 0) || (num `mod` 5 == 0) 

main::IO()
main = do
    print $ sum $ filter isMult [1..999]
