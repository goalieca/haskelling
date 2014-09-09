--project euler problem 50
{--
The prime 41, can be written as the sum of six consecutive primes:
41 = 2 + 3 + 5 + 7 + 11 + 13

This is the longest sum of consecutive primes that adds to a prime below one-hundred.

The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.

Which prime, below one-million, can be written as the sum of the most consecutive primes?
--}

import Data.Numbers.Primes

-- brute force + unoptimized search
-- only fast because lazy eval..
-- runs in 2ms. highly dependant on size of list

list :: [Int]
list = takeWhile ((>=) 4000) primes

findSeq len xs
    | (length ys) < len = 0
    | (isPrime ss) = ss
    | otherwise = findSeq len (tail xs)
    where ys = take len xs
          ss = sum ys

-- i probably search longer list than needed this way but starting to look for lengths of len-x
-- first makes for a 100x faster exit.
main = print $ head $ [ y | x<-[0..len-1], let y = findSeq (len - x) list, y > 0, y < 1000000]
    where len = (length list)
