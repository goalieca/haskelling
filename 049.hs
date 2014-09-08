{--

    The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways: (i) each of the three terms are prime, and, (ii) each of the 4-digit numbers are permutations of one another.

    There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but there is one other 4-digit increasing sequence.

    What 12-digit number do you form by concatenating the three terms in this sequence?

--}

import Data.Numbers.Primes
import Data.List
import Data.Array.Unboxed

(*|) a b = a * 10 + b
undigits xs = foldl (*|) 0 xs

digits = reverse . map (`mod` 10) . takeWhile (> 0) . iterate (`div` 10)

is_permutation a b c = (b `elem` ps) && (c `elem` ps)
    where ps = map undigits (permutations (digits a))

-- easy to brute force such a small search space
funny_numbers :: [Int]
funny_numbers = [ concat3 a b c  | a <- [1000..9999], d <-[1..(9999-a) `div` 2], let b = a + d, let c = b + d, isPrime a, isPrime b, isPrime c, is_permutation a b c]
    where concat3 a b c = undigits $ (digits a) ++ (digits b) ++ (digits c)

main = print $ funny_numbers
