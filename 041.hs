{--
    We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is also prime.

    What is the largest n-digit pandigital prime that exists?
--}
import Data.List
import Data.Numbers.Primes
import qualified Data.HashSet as HashSet

(*|) a b = a * 10 + b
undigits xs = foldl (*|) 0 xs

-- 1 to 9 pandigitals
pandigitals = concat $ map (\n -> map undigits (permutations [1..n])) [1..9]

main = print $ maximum $ filter isPrime pandigitals
