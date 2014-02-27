-- project euler problem 23
{--
A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

A number whose proper divisors are less than the number is called deficient and a number whose proper divisors exceed the number is called abundant.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.

Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
--}

--okay, this is basically a brute force attack..
--but what do you expect at 2am after a long day of overwhelming
--work!

--wikipedia narrowed search space to 20161 :-)

proper_divisors n = [x | x<-[1..n `div` 2], n `mod` x == 0 ]
is_abundant n = (sum (proper_divisors n)) > n
abundants n = filter is_abundant [1..n]

abuns = abundants 20161
cant_sum n = null [x | x<-abunsS, y<-abunsS,x+y==n]
    where abunsS = takeWhile (<n) abuns

main = print $ sum $ filter cant_sum [1..20161]
