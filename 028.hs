{--
Problem 28

Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:

21 22 23 24 25
20  7  8  9 10
19  6  1  2 11
18  5  4  3 12
17 16 15 14 13

It can be verified that the sum of the numbers on the diagonals is 101.

What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?

--}

{--

    My strategy is semi-efficient and I hope that ghc can flatten the forrest

        I compute all the diagonals from the center to the top right
        Note that they are all squares :)

        Then I noted that the other 3 corners are related to those squares
        [n^2-(n-1)*m | m<-[0,1,2,3]

        Finally, I needed to find out how many corners to go up. 

--}

import Data.Map

-- n are the squares (diagonal going top right)
-- 500 because that's (1001-1)/2. work from center to top half. 
-- flatten that equation from above to a polynomial
diag = [ 4*n*n-6*n+6 | m<-[1..500], let n=(1+2*m) ]

-- add the center "1" box and sum the list of corners
main = print $ 1 + (sum diag)
