{--
,A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:

,   1/2 =   0.5
,   1/3 =   0.(3)
,   1/4 =   0.25
,   1/5 =   0.2
,   1/6 =   0.1(6)
,   1/7 =   0.(142857)
,   1/8 =   0.125
,   1/9 =   0.(1)
,   1/10,=   0.1

,   Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.

Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.
--}

{--
    This problem was best solved by doing a bit of math first.
    
    Since these are rational numbers being tested, we know there is either a terminating digit or a repeating decimal.
    If the number is coprime with 10, we can see that it can't be repeating (we write in base 10, just a matter of shiting decimal points
        to get a whole integer). 

    I suspected that it has to be a prime. Any other large number just factors down to smaller primes. A number x can only have up to 
    x-1 repeating digits (think about the process of long division). 
    
    This led me to find "Full Reptend Prime". It is a prime p for which 1/p has a maximal period of p-1 digits.

    What luck.. from wikipedia
    "The values of p less than 1000 for which this formula produces cyclic numbers in decimal are:
        7, 17, 19, 23, 29, 47, 59, 61, 97, 109, 113, 131, 149,
        167, 179, 181, 193, 223, 229, 233, 257, 263, 269, 313,
        337, 367, 379, 383, 389, 419, 433, 461, 487, 491, 499,
        503, 509, 541, 571, 577, 593, 619, 647, 659, 701, 709,
        727, 743, 811, 821, 823, 857, 863, 887, 937, 941, 953,
        971, 977, 983, ... (sequence A001913 in OEIS)"
--}

main = print 983
