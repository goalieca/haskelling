--project euler problem 15
{--
Starting in the top left corner of a 2×2 grid, there are 6 routes (without backtracking) to the bottom right corner.

How many routes are there through a 20×20 grid?
--}

{--
mathematically this is a trivial problem... pascal's triangle to the rescue!
 --}

factorial n = product [1..n]
m `choose` n = (factorial m) `div` ((factorial n) * (factorial (m-n)))

main = do print $ 40 `choose` 20
