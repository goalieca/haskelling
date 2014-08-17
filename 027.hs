--project euler problem 27
{--
Euler published the remarkable quadratic formula:

n² + n + 41

It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and certainly when n = 41, 41² + 41 + 41 is clearly divisible by 41.

Using computers, the incredible formula  n²  79n + 1601 was discovered, which produces 80 primes for the consecutive values n = 0 to 79. The product of the coefficients, 79 and 1601, is 126479.

Considering quadratics of the form:

n² + an + b, where |a| < 1000 and |b| < 1000

where |n| is the modulus/absolute value of n
e.g. |11| = 11 and |4| = 4
Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n = 0.
--}

import Data.Array
import Data.List

-- borrowed from 050.hs
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

-- magic eulerian formula
quadratic :: Int -> Int -> Int -> Int
quadratic a b n = n*n + a*n + b

-- [y] = f([x])
list :: Int -> Int -> [Int]
list a b = [ quadratic a b n | n <- [0..1000] ]

-- counts consectives
consecutive :: [Int] -> Int
consecutive list = length $ takeWhile (\x -> x `elem` (primes 1000)) list

-- generates all a*b and the number of consecutive primes
generator :: [(Int,Int)]
generator = [(a*b, consecutive (list a b)) | a<-[-999..999], b<-[-999..999]]

-- finds the (a*b, # consecutives)
findMax :: [(Int,Int)] -> (Int,Int)
findMax list = maximumBy comp list where
    comp (a,b) (c,d) = compare b d

-- :)
main = print $ findMax generator
