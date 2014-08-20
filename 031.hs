{--
    In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:

    1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
    It is possible to make £2 in the following way:

    1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
    How many different ways can £2 be made using any number of coins?
--}

{-- 
    Brute force is fast enough.. Next!

--}

main = print $ sum $ [ 1 | a<-[0,1],
                           b<-[0..2], 
                           c<-[0..4], 
                           d<-[0..10], 
                           e<-[0..20],
                           f<-[0..40],
                           g<-[0..100-(a*200+b*100+c*50+d*20+e*10+f*5)/2+1],
                           h<-[0..200-(a*200+b*100+c*50+d*20+e*10+f*5+g*2)],
                           a*200+b*100+c*50+d*20+e*10+f*5+g*2+h*1 == 200 ]
