{--

    The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each of the digits 0 to 9 in some order, but it also has a rather interesting sub-string divisibility property.

        Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note the following:

        d2d3d4=406 is divisible by 2
        d3d4d5=063 is divisible by 3
        d4d5d6=635 is divisible by 5
        d5d6d7=357 is divisible by 7
        d6d7d8=572 is divisible by 11
        d7d8d9=728 is divisible by 13
        d8d9d10=289 is divisible by 17
        Find the sum of all 0 to 9 pandigital numbers with this property.

--}
import Data.List
import Data.Numbers.Primes
import Data.Int

(*|) a b = a * 10 + b
undigits xs = foldl (*|) 0 xs

pandigitals::[[Int64]]
pandigitals = permutations [0..9]

mini_list :: [Int64] -> [Int64]
mini_list (x:xs) = map undigits $ take 7 $ map (take 3 ) $ iterate tail xs

funny_number :: [Int64] -> Bool
funny_number xs = all (== 0) $ zipWith mod (mini_list xs) (primes)

main = print $ sum $ map undigits $ filter funny_number pandigitals
