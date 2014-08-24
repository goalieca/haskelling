{--

    The number 3797 has an interesting property. Being prime itself, it is possible to continuously remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.

    Find the sum of the only eleven primes that are both truncatable from left to right and right to left.

    NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

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

truncate_left x = map undigits (take (length xs) (iterate tail xs))
            where xs = digits x

truncate_right x = take (length xs) (map (undigits . reverse) (iterate tail (reverse xs)))
            where xs = digits x

truncatable n = HashSet.filter (\x -> (x > 10) && (is_left x) && (is_right x)) primes
            where   primes = HashSet.fromList (primes_gen n)
                    is_left x = all (\y-> y `HashSet.member` primes) (truncate_left x)
                    is_right x = all (\y-> y `HashSet.member` primes) (truncate_right x)

main = print $ HashSet.foldl' (+) 0 (truncatable 1000000)
