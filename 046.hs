{--

    It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a square.

        9 = 7 + 2×1^2
        15 = 7 + 2×2^2
        21 = 3 + 2×3^2
        25 = 7 + 2×3^2
        27 = 19 + 2×2^2
        33 = 31 + 2×1^2

    It turns out that the conjecture was false.
    What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?

--}
import Data.Numbers.Primes
import Math.NumberTheory.Powers.Squares

is_composite = not . isPrime

odds = iterate (+2) 1

-- starts at 3
odd_composites :: [Int]
odd_composites = [x | x<-(tail odds), is_composite x]

-- x = p + 2*z^2
-- square_root ((x-p)/2) = integer implies this is a funny_number
main = print $ head $ [x | x <- odd_composites, funny_number x]
    where funny_number x = null [True | y <- (takeWhile (< x) primes), let z = exactSquareRoot ((x-y) `div` 2), z /= Nothing]

