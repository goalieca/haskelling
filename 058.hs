{--

    Starting with 1 and spiralling anticlockwise in the following way, a square spiral with side length 7 is formed.

        37 36 35 34 33 32 31
        38 17 16 15 14 13 30
        39 18  5  4  3 12 29
        40 19  6  1  2 11 28
        41 20  7  8  9 10 27
        42 21 22 23 24 25 26
        43 44 45 46 47 48 49

        It is interesting to note that the odd squares lie along the bottom right diagonal, but what is more interesting is that 8 out of the 13 numbers lying along both diagonals are prime; that is, a ratio of 8/13 ≈ 62%.

        If one complete new layer is wrapped around the spiral above, a square spiral with side length 9 will be formed. If this process is continued, what is the side length of the square spiral for which the ratio of primes along both diagonals first falls below 10%?

--}

{--

    Problem 28 generated a spiral much like this except flipped vertically.

    It's too easy they noted that the one diagonal are squares 1^2,3^2,5^2,7^2
        n = [1+2*i | i<-[0..]]
    The other corners are related to that square by [n^2-(n-1)*m | m<-[0,1,2,3]

    Now, the length of the side once those stop being primes is going to be simply
    the subtraction of 2 corners (m=1 and m=0, -> (n^2 - (n^2-(n-1)) + 1 => n :)

    Assume 1 is not a diagonal
--}
import Data.List
import Math.NumberTheory.Primes.Testing

-- really should be in prelude
count f list = length $ filter f list

--i is loop variable, (d:ds) are diagonals, primes = number so far
keep_going (d:ds) = _rec 1 (d:ds) 0
    where _rec i (d:ds) primes
            | (primes + (count isPrime d)) * 10 > i * 4 = _rec (i+1) ds (primes + (count isPrime d))
            | otherwise = (head d) - (head (tail d)) + 1

main = print $ keep_going [diags (1+2*i) | i<-[1..]] 
    where diags n = [n*n-(n-1)*m | m<-[0..3]]
