--project euler problem 22
{--
Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.

What is the total of all the name scores in the file?
--}
import Char
import List

inner_product a b = sum $ zipWith (*) a b

name_sum str = sum $ map index str
    where index x = (ord . toLower) x - 96

--just here to make words work nicely
whitespace x
    | x == ',' = ' '
    | otherwise = x

main = do
    input <- readFile "names.txt"
    let filtered = words $ map whitespace $ filter (/='"') input
    let sums = map name_sum (sort filtered)
    print $ inner_product sums [1..]
