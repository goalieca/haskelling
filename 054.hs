{--

In the card game poker, a hand consists of five cards and are ranked, from lowest to highest, in the following way:

    High Card: Highest value card.
    One Pair: Two cards of the same value.
    Two Pairs: Two different pairs.
    Three of a Kind: Three cards of the same value.
    Straight: All cards are consecutive values.
    Flush: All cards of the same suit.
    Full House: Three of a kind and a pair.
    Four of a Kind: Four cards of the same value.
    Straight Flush: All cards are consecutive values of same suit.
    Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
    The cards are valued in the order:
    2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.

    If two players have the same ranked hands then the rank made up of the highest value wins; for example, a pair of eights beats a pair of fives (see example 1 below). But if two ranks tie, for example, both players have a pair of queens, then highest cards in each hand are compared (see example 4 below); if the highest cards tie then the next highest cards are compared, and so on.

    Consider the following five hands dealt to two players:

            <snip>

    The file, poker.txt, contains one-thousand random hands dealt to two players. Each line of the file contains ten cards (separated by a single space): the first five are Player 1's cards and the last five are Player 2's cards. You can assume that all hands are valid (no invalid characters or repeated cards), each player's hand is in no specific order, and in each hand there is a clear winner.

   How many hands does Player 1 win?

--}
import Data.List

data Value = Two | Three | Four | Five | Six | Seven 
        | Eight | Nine | Ten | Jack | Queen | King | Ace
        deriving (Ord, Eq, Read, Show, Enum)

data Suit = Club | Spade | Diamond | Heart
        deriving (Eq, Read, Show)

-- ord explicit to avoid suit comparison
data Card = Card { value :: Value, suit :: Suit }
        deriving (Read, Show)

instance Eq Card where
        (==) a b = (value a) == (value b)

instance Ord Card where
        compare a b = compare (value a) (value b)

data Hand = High [Card] | OnePair [Card] | TwoPair [Card] | ThreeKind [Card]
            | Straight [Card] | Flush [Card] | FullHouse [Card]
            | FourKind [Card] | StraightFlush [Card] | RoyalFlush [Card]
            deriving (Eq, Ord, Read, Show)

-- will return cards sorted highest-lowest, except pairs first (3 pair before 2 pair)
processHand cards
    | all_suit && in_sequence && ((value (head sorted)) == Ace) = RoyalFlush sorted
    | all_suit && in_sequence = StraightFlush sorted
    | length (head paired) == 4 = FourKind (concat paired)
    | ((length (paired!!0)) == 3) && ((length (paired!!1)) == 2) = FullHouse (concat paired)
    | all_suit = Flush sorted
    | in_sequence = Straight sorted
    | (length (paired!!0)) == 3 = ThreeKind (concat paired)
    | ((length (paired!!0)) == 2)  && ((length (paired!!1)) == 2) = TwoPair (concat paired)
    | length (paired!!0) == 2 = OnePair (concat paired)
    | otherwise = High sorted 
    where sorted = sortBy (flip compare) cards
          all_suit = all (\x -> (suit x) == (suit (head sorted))) sorted
          in_sequence = (map value (reverse sorted)) `isInfixOf` [Two .. ]
          paired = sortBy (\x y -> compare (length y) (length x)) (group sorted)

readCard (v:s:[]) = Card (fromValue v) (fromSuit s)
    where
        fromValue v
            | v == '2' = Two
            | v == '3' = Three
            | v == '4' = Four
            | v == '5' = Five
            | v == '6' = Six
            | v == '7' = Seven
            | v == '8' = Eight
            | v == '9' = Nine
            | v == 'T' = Ten
            | v == 'J' = Jack
            | v == 'Q' = Queen
            | v == 'K' = King
            | v == 'A' = Ace
        fromSuit s
            | s == 'C' = Club
            | s == 'S' = Spade
            | s == 'D' = Diamond
            | s == 'H' = Heart

processGame (p1,p2) = compare p1 p2

readGame line = (hand1 , hand2)
    where
        cards = words line
        p1 = take 5 cards
        p2 = drop 5 cards
        hand1 = processHand (map readCard p1)
        hand2 = processHand (map readCard p2)

runGames games = map (== GT) $ map (processGame.readGame) games

main = do
        dat <- readFile "p054_poker.txt"
        print $ length $ filter (== GT) $ map (processGame.readGame) (lines dat)
