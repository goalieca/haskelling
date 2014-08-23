--project euler problem 35
{--
The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.

There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

How many circular primes are there below one million?
--}

import Data.Array
import qualified Data.HashSet as HashSet

--copied from 050.hs (the bottleneck of the whole operation!!)
primes_gen :: Int -> [Int]
{-# NOINLINE primes_gen #-}
primes_gen n 
  | n <= 2    = []
  | otherwise = 
    let
      sqrPrimes = primes_gen (ceiling (sqrt (fromIntegral n)))
      sieves    = concat
            [[2 * p, 3 * p..n - 1] | p <- sqrPrimes]
      sieves'   = zip sieves (repeat False)
      flags     = accumArray (&&) True (0, n - 1) sieves'
    in
    drop 2 (filter (flags!) [0..n - 1])

--copied from 034.hs and reversed to be in right order
digits = reverse . map (`mod` 10) . takeWhile (> 0) . iterate (`div` 10)

--handles reverse ordered list from digits
(*|) a b = a * 10 + b
undigits xs = foldl (*|) 0 xs

--rotate a reversed list with this still rotates it
rotate (x:xs) = xs ++ [x]

--cycles through every possible option
rotation_list x = map undigits $ take (length x) $ iterate rotate x

--now to find membership in primes list
-- TODO: redundant checks for circulars (should add all rotations after first one passes, not check again)
circular n = HashSet.filter (\x -> (all (\y-> y `HashSet.member` primes) (rotation_list (digits x)))) primes
                where primes = HashSet.fromList (primes_gen n)

--count number of primes :)
main = print $ HashSet.size $ circular 1000000
