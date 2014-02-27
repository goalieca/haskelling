--project euler problem 35
{--
The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.

There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

How many circular primes are there below one million?
--}

import Data.Array
import Data.Set

--copied from 050.hs
primes :: Int -> [Int]
{-# NOINLINE primes #-}
primes n 
  | n <= 2    = []
  | otherwise = 
    let
      sqrPrimes = primes (ceiling (sqrt (fromIntegral n)))
      sieves    = concat
            [[2 * p, 3 * p..n - 1] | p <- sqrPrimes]
      sieves'   = zip sieves (repeat False)
      flags     = accumArray (&&) True (0, n - 1) sieves'
    in
    drop 2 (filter (flags!) [0..n - 1])

--copied from 034.hs (comes out in reverse order.. but that's fine)
digits = map (`mod` 10) . takeWhile (> 0) . iterate (`div` 10)

--handles reverse ordered list from digits
undigits [] = 0
undigits x:xs = xs + (10 * undigits xs)

--rotate a reversed list with this still rotates it
rotate (x:xs) = xs ++ [x]

--cycles through every possible option
rotation_list x = take (length x) $ undigits $ rotate x

--now to find membership (throw into a hashmap to avoid linear search)
circle_primes
    where setOfPrimes = fromList primes


main = do
    print $ primes 1000000
