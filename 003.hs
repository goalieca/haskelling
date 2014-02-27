-- Project euler problem 3.

{--
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
--}

maxPrime::Integer->Integer->Integer->Integer
maxPrime number largest guess
    | guess > number = largest
    | number `mod` guess == 0 = maxPrime (number `div` guess) guess guess
    | otherwise = maxPrime number largest (guess+1) --should really only check odd numbers

main = print $ maxPrime 600851475143 1 2 
