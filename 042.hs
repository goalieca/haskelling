{--
    The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1); so the first ten triangle numbers are:

    1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

    By converting each letter in a word to a number corresponding to its alphabetical position and adding these values we form a word value. For example, the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value is a triangle number then we shall call the word a triangle word.

    Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing nearly two-thousand common English words, how many are triangle words?

--}

import qualified Data.HashSet as HashSet
import Data.List.Split
import Data.Char

-- 20 was chosen as 20*20 / 26 should be more than long enough
triangles = HashSet.fromList [n*(n+1) `div` 2 | n <- [1..20]]

sum_word word = sum $ map (\x -> (ord x) - 64) word

parse txt = wordsBy (==',') $ filter ((/=) '"') txt

main = do
        contents <- readFile "p042_words.txt"
        print $ length $ filter (\x -> x `HashSet.member` triangles) $ map sum_word $ parse contents
