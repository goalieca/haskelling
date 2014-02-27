--project euler problem 17
{--
If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used? 

NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.
--}

digits = ["","one","two","three","four","five","six","seven","eight","nine"]
digits_teens = ["ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"]
digits_10 = ["","ten","twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]

--this function is the result of a long day and a 2am quick hack to help fall asleep
show_words x
    | x < 10 = digits!!x
    | x >= 10 && x < 20 = digits_teens!!(x - 10)
    | x < 100 = digits_10!!(x `div` 10) ++ digits!!(x `mod` 10)
    | x < 1000 && x `mod` 100 == 0 = digits!!(x `div` 100) ++ "hundred"
    | x < 1000 = (show_words (x - (x `mod` 100))) ++ "and" ++ (show_words (x `mod` 100))
    | x == 1000 = "onethousand"
    | otherwise = ""

main = print $ length $ concatMap show_words [1..1000]
