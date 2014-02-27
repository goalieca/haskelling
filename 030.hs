--project euler problem 30
{--
Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:

    1634 = 1^(4) + 6^(4) + 3^(4) + 4^(4)
    8208 = 8^(4) + 2^(4) + 0^(4) + 8^(4)
    9474 = 9^(4) + 4^(4) + 7^(4) + 4^(4)

As 1 = 1^(4) is not a sum it is not included.

The sum of these numbers is 1634 + 8208 + 9474 = 19316.

Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.
--}
import Char

can_5 x = (sum (map (^5) nums)) == x
    where nums = map digitToInt (show x)

--how did i know 6? well i just tried em all until it would search too long
--doing real math would hurt me at this point this late at night
--this only took me a few minutes to solve :-)
main = print $ sum $ take 6 $ filter can_5 [2..]
