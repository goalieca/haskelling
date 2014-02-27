--project euler problem 390
{--
Consider the triangle with sides sqrt(5), sqrt(65) and sqrt(68). It can be shown that this triangle has area 9.

S(n) is the sum of the areas of all triangles with sides sqrt(1+b2), sqrt(1+c2) and sqrt(b2+c2) (for positive integers b and c ) that have an integral area not exceeding n.

The example triangle has b=2 and c=8.

S(10^6)=18018206.

Find S(10^10).
--}

area_triangle_sq a b c = s * (s - a) * (s - b) * (s-c)
    where s = (a+b+c)/2

main = do
    txt <- readFile "base_exp.txt"
    let pairs =  zip [1..] $ map (split ',') $ lines txt
    print $ fst $ maximumBy cmp pairs
