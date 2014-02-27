-- project euler problem 4
{--
A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.
--}

isPalindrome x = (y == reverse y)
    where y = show x

scalarMult c v = map (\x -> c*x) v
mixMult a b = foldr (\x y -> (scalarMult x b)++y) [] a

main = print $ maximum $ filter isPalindrome $ mixMult [100..999] [100..999]
