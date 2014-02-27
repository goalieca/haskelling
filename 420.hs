--project euler problem 420
{--
A positive integer matrix is a matrix whose elements are all positive integers.
Some positive integer matrices can be expressed as a square of a positive integer matrix in two different ways. Here is an example:

[ 40 12 ] = [2  3]^2 = [6 1]^2
[ 48 40 ]   [12 2]     [4 6]


We define F(N) as the number of the 2x2 positive integer matrices which have a trace less than N and which can be expressed as a square of a positive integer matrix in two different ways.
We can verify that F(50) = 7 and F(1000) = 1019.

Find F(107).
--}

import Data.List

matrices = [(a,b,c,d) | a<-[1..32], b<-[1..32], c<-[1..32], d<-[1..32], a*a+d*d+2*b*c < 1000, a*b-c*d > 0]

--matrices = [(a*a+b*c,a*b+d*b,a*c+d*c,d*d+b*c) | a<-[1..32], b<-[1..175], c<-[1..175], d<-[1..sqrt(1000-a*a-2*b*c)+1] , a*a+2*b*c+d*d < 1000 ] 

rle = map (\xs -> (head xs, length xs)) . group . sort

main = do
    print $ length $ filter (\(_,x) -> x == 2) $ rle $ matrices
