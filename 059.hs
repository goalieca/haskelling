{--
    Each character on a computer is assigned a unique code and the preferred standard is ASCII (American Standard Code for Information Interchange). For example, uppercase A = 65, asterisk (*) = 42, and lowercase k = 107.

    A modern encryption method is to take a text file, convert the bytes to ASCII, then XOR each byte with a given value, taken from a secret key. The advantage with the XOR function is that using the same encryption key on the cipher text, restores the plain text; for example, 65 XOR 42 = 107, then 107 XOR 42 = 65.

    For unbreakable encryption, the key is the same length as the plain text message, and the key is made up of random bytes. The user would keep the encrypted message and the encryption key in different locations, and without both "halves", it is impossible to decrypt the message.

    Unfortunately, this method is impractical for most users, so the modified method is to use a password as a key. If the password is shorter than the message, which is likely, the key is repeated cyclically throughout the message. The balance for this method is using a sufficiently long password key for security, but short enough to be memorable.

    Your task has been made easy, as the encryption key consists of three lower case characters. Using cipher.txt (right click and 'Save Link/Target As...'), a file containing the encrypted ASCII codes, and the knowledge that the plain text must contain common English words, decrypt the message and find the sum of the ASCII values in the original text.

--}

import Data.Bits
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.HashSet as HashSet

-- when I first read this problem I got all excited, then i re-read it again after thinking about it in the shower only to zone
-- in on the encrypted using "three lower case characters". This is far too easy to brute force 26*26*26 = 17,576. Not worth
-- my time to do anything else

doRound text key = zipWith (\x y -> chr (xor (ord x) (ord y))) text (cycle key)

wordCount dictionary text = length $ filter (\x -> HashSet.member (map toLower x) dictionary) $ splitOneOf ";.,:!?\"\'() " text

keys = [[a,b,c] | a <- ['a'..'z'], b<-['a'..'z'], c<-['a'..'z']]

-- finds key
brute text dictionary keys = fst $ maximumBy (\x y -> compare (snd x) (snd y)) $ [(key,wordCount dictionary (doRound text key)) | key <- keys]

main = do
    dictionaryInput <- readFile "wordsEn.txt"
    textInput <- readFile "p059_cipher.txt"
    let text = map (chr . read) (splitOn "," textInput)
    let dictionaryWords = HashSet.fromList (lines dictionaryInput)
    let winningKey = brute text dictionaryWords keys
    --print $ doRound text winningKey
    print $ sum $ map ord $ doRound text winningKey
