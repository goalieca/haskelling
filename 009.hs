--project euler problem 9
{--
A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
a^(2) + b^(2) = c^(2)

For example, 3^(2) + 4^(2) = 9 + 16 = 25 = 5^(2).

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
--}
isTriple (a,b) = a + b + c == 1000 && a /= b
    where c = sqrt $ a*a + b*b

findTriple = filter isTriple [ (x,y) | x<-[1..1000], y<-[1..1000] ]

findProduct = a*b*c
    where (a,b) = head findTriple
          c = sqrt $ a*a + b*b

main = print $ round $ findProduct
