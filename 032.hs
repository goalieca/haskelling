--project euler problem 32
{--
We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.

The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.

Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.
--}
import List

--has all number 1-9 and has 9 digits
isPandigital a b = len_chk && elem_chk
        where len_chk = 9 == (length num)
              elem_chk = all (\x-> x `elem` num) ['1'..'9']
              num = (show a) ++ (show b) ++ (show (a*b))

main = print $ sum $ nub $ [a*b | a<-[1..10000],b<-[1..term a], isPandigital a b]
    where term a
            | a >= 1000 = 10
            | a >= 100 = 100
            | a >= 10 = 1000
            | otherwise = 10000
