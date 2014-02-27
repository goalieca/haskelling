--project euler problem 56
{--
A googol (10^(100)) is a massive number: one followed by one-hundred zeros; 100^(100) is almost unimaginably large: one followed by two-hundred zeros. Despite their size, the sum of the digits in each number is only 1.

Considering natural numbers of the form, a^(b), where a, b < 100, what is the maximum digital sum?
--}
import Char

main = print $ maximum [sum $ map digitToInt $ show (a^b) | a<-[1..99], b<-[95..99]]
