{--
    An irrational decimal fraction is created by concatenating the positive integers:

        0.123456789101112131415161718192021...

    It can be seen that the 12th digit of the fractional part is 1.

    If dn represents the nth digit of the fractional part, find the value of the following expression.

        d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000

--}

--copied from 034.hs and reversed to be in right order
digits = reverse . map (`mod` 10) . takeWhile (> 0) . iterate (`div` 10)

-- takes 2 digits and smashes them into a list
smash x n = (digits x)++[n]

list::[Int]
list = [1,2,3,4,5,6,7,8,9] ++ (concat [ smash x n |  x<-[1..], n<-[0..9]])

-- lazy eval FTW
main = print $ foldl (*) 1 $ map d [1,10,100,1000,10000,100000,1000000]
                             where d n = list!!(n-1)
