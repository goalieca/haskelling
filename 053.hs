--project euler problem 53
{--
There are exactly ten ways of selecting three from five, 12345:
123, 124, 125, 134, 135, 145, 234, 235, 245, and 345
It is not until n = 23, that a value exceeds one-million: ^(23)C_(10) = 1144066.

How many, not necessarily distinct, values of  ^(n)C_(r), for 1 ≤ n ≤ 100, are greater than one-million?
--}

fac 0 = 1
fac n = n * (fac (n-1))

combination n r = (fac n) `div` ((fac r)*(fac (n-r)))

main = print $ length $ filter (\x -> x>1000000) [ (combination n r) | n<-[23..100], r<-[1..n]]
