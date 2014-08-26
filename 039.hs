{--
    If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.

        {20,48,52}, {24,45,51}, {30,40,50}

    For which value of p ≤ 1000, is the number of solutions maximised?
--}

import Data.List

-- returns a list of p's to save computation later in finding length
list_for_p :: Int -> [Int]
list_for_p p = [p | a<-[1..p], b<-[a..p], let c = p - a - b, a*a + b*b == c*c]

main = print $ head $ maximumBy (\x y -> compare (length x) (length y)) $ map list_for_p [1..1000]
