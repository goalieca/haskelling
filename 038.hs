{--

    Take the number 192 and multiply it by each of 1, 2, and 3:

        192 × 1 = 192
        192 × 2 = 384
        192 × 3 = 576
    
    By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576 the concatenated product of 192 and (1,2,3)

    The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).

    What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an integer with (1,2, ... , n) where n > 1?

--}
import Data.List
import qualified Data.HashSet as HashSet

--handles reverse ordered list from digits
(*|) a b = a * 10 + b
undigits xs = foldl (*|) 0 xs

-- 1 to 9 pandigitals.. all! sorted from largest to smallest
is_pandigital::Int->Bool
is_pandigital x = x `HashSet.member` pandigitals
        where pandigitals = HashSet.fromList (map undigits (permutations [1..9]))

-- make a candidate number
nums::Int->Int
nums x = read (take 9 $ concat $ map (show . (*) x) [1..])::Int

main = print $ maximum $ filter is_pandigital $ map nums [1..9999]
