--project eulere problem 12
{--
The sequence of triangle numbers is generated by adding the natural numbers. So the 7^(th) triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The first ten terms would be:

1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

Let us list the factors of the first seven triangle numbers:

     1: 1
     3: 1,3
     6: 1,2,3,6
    10: 1,2,5,10
    15: 1,3,5,15
    21: 1,3,7,21
    28: 1,2,4,7,14,28

We can see that 28 is the first triangle number to have over five divisors.

What is the value of the first triangle number to have over five hundred divisors?
--}

--very elegant (except isqrt)
triangle_nums::[Int]
triangle_nums = [ sum [1..x] | x<-[1..] ]

isqrt::Int->Int
isqrt x = floor $ sqrt $ fromIntegral x

factors::Int->[Int]
factors x = [ y | y<-[1..isqrt x], (mod x y) == 0 ]

num_div::Int->Int
num_div x = 2 * (length (factors x))

find_first::Int->Int
find_first num = head $ filter (\x -> num_div x >= num) triangle_nums

main = print $ find_first 500
