--project euler problem 18
import Monad

pairUp row = (start,start) : (zip row (tail row)) ++ [(end,end)]
    where start = head row
          end = row!!(length row - 1)

sumRow prev current = zipWith sumIt (pairUp prev) current
    where sumIt (a,b) c = (max a b) + c

sumTriangle rows [] = sumTriangle (tail rows) (head rows)
sumTriangle [] prev = prev
sumTriangle rows prev = sumTriangle (tail rows) (sumRow prev (head rows))

main = do
        input <- getContents
        let triangle = map (\x -> map read (words x)) (lines input)
        let final_sum = sumTriangle triangle []
        print (maximum final_sum)
