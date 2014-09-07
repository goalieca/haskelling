{--

    The first two consecutive numbers to have two distinct prime factors are:

        14 = 2 × 7
        15 = 3 × 5

    The first three consecutive numbers to have three distinct prime factors are:

        644 = 2² × 7 × 23
        645 = 3 × 5 × 43
        646 = 2 × 17 × 19.

    Find the first four consecutive integers to have four distinct prime factors. What is the first of these numbers?

--}

import Data.List
import qualified Data.Set as Set
import Data.Numbers.Primes

multiply = foldl' (*) 1

factored :: [[Int]]
factored = [ primeFactors  x | x <- [2..] ]

num_unique = length . nub

-- fast exit. very proud of this one :)
find4 (a:b:c:d:es) 
    | (num_unique d) /= 4 = find4 (es)
    | (num_unique c) /= 4 = find4 (d:es)
    | (num_unique b) /= 4 = find4 (c:d:es)
    | (num_unique a) == 4 = a
    | otherwise = find4 (b:c:d:es)

main = print $ multiply $ find4 factored
