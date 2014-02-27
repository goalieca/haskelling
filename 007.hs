-- project euler problem 7
{--
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6^(th) prime is 13.

What is the 10001^(st) prime number?
--}

--sieve of erathosthenes
sieve (x:xs) 1 = x
sieve (x:xs) cnt = sieve [y | y<- xs, y `mod` x > 0] (cnt-1)

main = print $ sieve [2..] 10001
