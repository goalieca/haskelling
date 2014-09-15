{--

    It is possible to show that the square root of two can be expressed as an infinite continued fraction.

        √ 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...

        By expanding this for the first four iterations, we get:

        1 + 1/2 = 3/2 = 1.5
        1 + 1/(2 + 1/2) = 7/5 = 1.4
        1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
        1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...

        The next three expansions are 99/70, 239/169, and 577/408, but the eighth expansion, 1393/985, is the first example where the number of digits in the numerator exceeds the number of digits in the denominator.

        In the first one-thousand expansions, how many fractions contain a numerator with more digits than denominator?

--}
num_digits :: Integer -> Int
num_digits x = _rec 1 10
    where _rec s y
            | y < x = _rec (s+1) (y*10)
            | otherwise = s

-- reduced an+1 = 1 + 1/(1 + an) to 
func (p,q) = ((2*q + p),q+p)

main = print $ length $ filter dig_comp $ take 1000 $ iterate func (3,2)
    where dig_comp (p,q) = (num_digits p) > (num_digits q)
