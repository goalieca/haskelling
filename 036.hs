--problem 36
{--
The decimal number, 585 = 1001001001_(2) (binary), is palindromic in both bases.

Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.
--}
import Numeric
import Char

showBin x = showIntAtBase 2 intToDigit x ""
palindrome x = x == (reverse x)
palindrome2 = palindrome . showBin
palindrome10 = palindrome . show

--optimize out even numbers (no leading 0's count)
main = print $ sum $ filter palindromic [1..999999]
    where palindromic x = (odd x) && (palindrome2 x) && (palindrome10 x)
