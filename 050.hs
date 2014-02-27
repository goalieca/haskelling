--project euler problem 50
{--
The prime 41, can be written as the sum of six consecutive primes:
41 = 2 + 3 + 5 + 7 + 11 + 13

This is the longest sum of consecutive primes that adds to a prime below one-hundred.

The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.

Which prime, below one-million, can be written as the sum of the most consecutive primes?
--}

import Data.Array

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

sumWhile n (x:xs) accum len
    | null (x:xs) = (accum,len)
    | accum < n = sumWhile n xs (x+accum) (len+1)
    | otherwise = (accum,len)

consecutive n primes = map
    let (accum,len) = sumWhile n primes 0 0

main = print $ maximum $ consecutive

--main = print (primes 1000000)
