--project euler problem 14

{--
The following iterative sequence is defined for the set of positive integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:
13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.
--}

genNext n
    | odd n = 3*n+1
    | otherwise = n `div` 2

chainLength n 
    | n == 1 = 1
    | otherwise = 1 + (chainLength . genNext) n

--position with maximum
maximum2 (max,maxpos) pos list
    | list == [] = maxpos
    | pos == 0 || max < (head list) = maximum2 (head list,pos) (pos+1) (tail list)
    | otherwise = maximum2 (max,maxpos) (pos+1) (tail list)

main = print $ (maximum2 (0,0) 0 $ map chainLength [1..1000000]) + 1
