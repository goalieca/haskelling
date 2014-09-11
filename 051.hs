{--

    By replacing the 1st digit of the 2-digit number *3, it turns out that six of the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.

    By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit number is the first example having seven primes among the ten generated numbers, yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993. Consequently 56003, being the first member of this family, is the smallest prime with this property.

   Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) with the same digit, is part of an eight prime value family.

--}

{--
        This was a crude solution rather than elegant math!

        Going to start assuming 6-digit number. We need 8 digits working from 10-digit number.
        Last digit cannot be even so no replacement of F.

        Started by checking 2 replacement digits.. that didn't work. Try 3 replacement digits.
--}

import Data.Numbers.Primes
import qualified Data.List as List
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable

digits :: Int -> [Int]
digits = reverse . map (`mod` 10) . takeWhile (> 0) . iterate (`div` 10)

(*|) :: Int -> Int -> Int
(*|) a b = a * 10 + b

undigits xs = Foldable.foldl' (*|) 0 xs

-- generate all possible prime combinations m < n < o
permutations (m,n,o) number = filter isPrime [ undigits (Seq.update m x (Seq.update n x (Seq.update o x ns ))) | x <- [s..9] ]
                where ns = Seq.fromList number
                      s = if m == 0 then 1 else 0

--funny_number :: Int -> Bool
funny_number number = any (elem number) filtered
    where ns = digits number
          len = length ns
          series = [permutations (m,n,o) ns | m<-[0..len-2], n<-[m+1..len-2], o<-[m+1..len-2], m<n, n<o]
          filtered = filter (\x -> length x == 8) series

-- 6 digit candidates (Stop condition is for stopping runaway compute times during testing)
candidates :: [Int]
candidates = takeWhile (< 1000000) $ dropWhile (< 100000) primes

main = print $ List.find funny_number candidates
