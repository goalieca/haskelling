--project euler problem 20
{--
n! means n × (n − 1) × ... × 3 × 2 × 1

Find the sum of the digits in the number 100!
--}
import Char

fac 0 = 1
fac n = n * (fac (n-1))

main = print $ sum $ map digitToInt $ show (fac 100)
