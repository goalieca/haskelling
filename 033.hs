{--

    The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.

    We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

    There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing two digits in the numerator and denominator.

    If the product of these four fractions is given in its lowest common terms, find the value of the denominator.

--}

-- for fraction type
import Data.Ratio

-- my own operator to make a number from 2 digits :)
(*|) a b = a * 10 + b

main = print $ denominator $ product [ a%c | c<-[2..9], a<-[1..c-1], b<-[1..9], (a *| b) * c == a * ( b *| c) ]
