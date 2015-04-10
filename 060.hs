{--

    The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and concatenating them in any order the result will always be prime. For example, taking 7 and 109, both 7109 and 1097 are prime. The sum of these four primes, 792, represents the lowest sum for a set of four primes with this property.

    Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.

--}

-- not going to lie, this is ugly but got it to run in 25s
-- sometimes it's more important to ship code than find mathematical beauty.
-- strategy:
--  a) generate a list of all primes except 2 and 5 since those will never work
--  b) compute all choices of 2 pairs and filter out non prime concatenations
--  c) all this to a multi-map (so we can quickly check if a prime pairs with another)
--  d) for each key in multimap generate combinations of 4 from values, find if those 4 are concat, filter out rest, sum values + add key
--  e) pick head of the list. why? smallest elements first of course. no proof, just hunch this is correct.

import Data.Numbers.Primes
import Data.List
import Data.MultiMap
import Data.Tuple

digits :: Int -> [Int]
digits = reverse . Data.List.map (`mod` 10) . takeWhile (> 0) . iterate (`div` 10)

(*|) a b = a * 10 + b
undigits xs = Data.List.foldl (*|) 0 xs

isPair (a,b) = (isPrime (undigits (_a++_b))) && (isPrime (undigits (_b++_a)))
    where _a = digits a
          _b = digits b

-- returns a list of all combinations
-- always chose first item, combine with combinations of rest of list
-- then concat that with the list of combinations if you didn't chose first
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = [x:cs | cs <- combinations (n-1) xs] ++ (combinations n xs)

all_pairs xs = all (\x -> (head x) `elem` (Data.MultiMap.lookup (head $ tail x) prime_pair_map)) (combinations 2 xs)

toTuple (x:xs) = (x,head xs)

num = 5
limit = 10000

-- drop 2,5 since those will never be prime in a pair
primes_list :: [Int]
primes_list = 3 : (drop 3 $ takeWhile (<limit) primes)

filtered_primes = Data.List.filter isPair $ Data.List.map toTuple $ combinations 2 primes_list

prime_pair_map = Data.MultiMap.fromList $ (Data.List.map Data.Tuple.swap filtered_primes) ++ filtered_primes

matching key mmap = Data.List.map (+key) $ Data.List.map sum $ filter all_pairs $ combinations (num-1) (Data.MultiMap.lookup key mmap)

main = print $ head $ concat $ Data.List.map (\x -> matching x prime_pair_map) $ Data.MultiMap.keys prime_pair_map
