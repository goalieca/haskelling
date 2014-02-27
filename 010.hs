--project euler problem 10
{--
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
--}

--sieve from 
--http://en.literateprograms.org/Special:Downloadcode/Sieve_of_Eratosthenes_(Haskell)
merge :: (Ord a) => [a] -> [a] -> [a]
merge xs@(x:xt) ys@(y:yt) = 
  case compare x y of
    LT -> x : (merge xt ys)
    EQ -> x : (merge xt yt)
    GT -> y : (merge xs yt)


diff :: (Ord a) => [a] -> [a] -> [a]
diff xs@(x:xt) ys@(y:yt) = 
  case compare x y of
    LT -> x : (diff xt ys)
    EQ -> diff xt yt
    GT -> diff xs yt


primes, nonprimes :: [Integer]
primes    = [2, 3, 5] ++ (diff [7, 9 ..] nonprimes) 
nonprimes = foldr1 f . map g . tail $ primes
  where 
    f (x:xt) ys = x : (merge xt ys)
    g p         = [ n * p | n <- [p, p + 2 ..]]

--disclaimer.. i only wrote everything underneath this
--the above is taken from haskell website. 
--sieve algorithms are not my interest!

take2 (x:xs)
    | x >= 2000000 = []
    | otherwise = x : (take2 xs)

main = print $ sum $ take2 primes
