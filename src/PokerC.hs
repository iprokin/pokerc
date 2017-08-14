{-
    Copyright 2017 Ilya Prokin

    This file is part of pokerc.

    pokerc is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    pokerc is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with pokerc.  If not, see <http://www.gnu.org/licenses/>.
-}

module PokerC 
    ( test
    ) where

import Data.Function
import System.Random.Shuffle
import Data.List (sort)
import Data.List (sortBy)
import Data.Ord (comparing)

data Rank = Two | Three | Four | Five | Six | Seven
          | Eight | Nine | Ten | Jack | Queen | King | Ace
          deriving (Eq, Ord, Bounded, Enum)
instance Show Rank where
    show x = case x of
               Two   -> "2"
               Three -> "3"
               Four  -> "4"
               Five  -> "5"
               Six   -> "6"
               Seven -> "7"
               Eight -> "8"
               Nine  -> "9"
               Ten   -> "T"
               Jack  -> "J"
               Queen -> "Q"
               King  -> "K"
               Ace   -> "A"

data Suit = Clubs | Diamonds | Hearts | Spades
    deriving (Eq, Bounded, Enum)
instance Show Suit where
    show x = case x of
               Clubs    -> "♣ "
               Diamonds -> "♦ "
               Hearts   -> "♥ "
               Spades   -> "♠ "


data Card = Card
  { rank :: Rank
  , suit :: Suit
  } deriving Eq
instance Ord Card where
  compare = compare `on` rank
instance Show Card where
  show (Card r s) = show r ++ show s

getSuit Card {rank=_, suit=s} = s
getRank Card {rank=r, suit=_} = r

allSuits = [Clubs, Diamonds, Hearts, Spades]
allRanks = [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]
deck = foldl (++) [] $ map (\s -> map (flip Card s) allRanks) allSuits

removeFromDeck cards deck = filter (\c -> all (/=c) cards) deck

giveCardsToPlayers _ _ [] = []
giveCardsToPlayers 0 _ _  = []
giveCardsToPlayers nPlayers nCards deck = 
    [take nCards deck] ++ giveCardsToPlayers (nPlayers-1) nCards (drop nCards deck)

countSuits cards = map (\suit -> length $ filter (\x -> getSuit x == suit) $ cards) allSuits

isFlushC cards = any (>=5) $ countSuits cards
isFlush comm hand = isFlushC (hand++comm)

prdR x | r == Two  = Ace
       | otherwise = pred r
     where r = getRank x

isStraight m comm hand = searchS 1 [firstCard] firstCard otherCards
    where firstCard      = head cardsProcessed
          otherCards     = tail cardsProcessed
          cardsProcessed = cardsRevSorted ++ (filterA Ace cardsRevSorted)
          cardsRevSorted = reverse $ sort (hand ++ comm)
          filterA a = filter (\x -> getRank x == a)
          searchS _ res _ [] = res
          searchS n res prev (x:xs)
            | n < m && sequentialRank = searchS (n+1) (x:res) x xs
            | sameRank                = searchS n (x:res) x xs
            | n >=m && not sameRank   = res
            | otherwise               = searchS 1 [x] x xs
            where sameRank       = getRank prev == getRank x
                  sequentialRank = prdR prev == getRank x

isStraightFlush comm hand = isFlushC $ isStraight 5 comm hand

isFlushRoyal comm hand = (highestCardRank == Ace) && isFlushC straight
    where straight = isStraight 5 comm hand
          highestCardRank = getRank $ head straight
{--
isStraight m comm hand = searchS 1 [] cardsProcessed
    where cardsProcessed = cardsRevSorted ++ (filterA Ace cardsRevSorted)
          cardsRevSorted = reverse $ sort (hand++comm)
          filterA a l = filter (\x -> getRank x == a) l
          searchS _ c [] = c
          searchS n c (x0:x1:[])
            | n < m-1                           = []
            | prdR x0 == getRank x1             = (x1:x0:c)
            | getRank x0 == getRank x1          = (x1:x0:c)
            | otherwise                         = []
          searchS n c (x0:x1:xs)
            | prdR x0 == getRank x1 && n < m-1  = searchS (n+1) (x0:c) (x1:xs)
            | prdR x0 == getRank x1 && n >= m-1 = (x1:x0:c)
            | getRank x0 == getRank x1          = searchS n (x0:c) (x1:xs)
            | otherwise                         = searchS 1 [] (x1:xs)
--}

pairsThreesFours :: [Card] -> [Card] -> [[Card]]
pairsThreesFours comm hand = reverse $ sortBy (comparing length) $ splitRank cardsSorted
    where cardsSorted = sort (hand ++ comm)
          splitRank (x:xs) = [x : takeWhile (sameRankAs x) xs] ++ splitRank (dropWhile (sameRankAs x) xs)
          splitRank []     = []
          sameRankAs x     = (== getRank x) . getRank

fullHouse :: [Card] -> [Card] -> [([Card],[Card])]
fullHouse c h = fullHousePTF $ pairsThreesFours c h
--fullHouse = fullHousePTF . pairsThreesFours

fullHousePTF :: [[Card]] -> [([Card],[Card])]
--fullHousePTF ptf = foldl (++) [] $ map (\p -> map (\t -> (t,p)) threes) pairs
fullHousePTF ptf = [(t,p) | t <- threes, p <- pairs]
    where threes = filter ((==3) . length) ptf
          pairs = filter ((==2) . length) ptf

test = do
    sdeck <- shuffleM deck
    putStrLn "Initial state of deck"
    print $ sdeck

    let nPlayers = 6
    let myCards = [Card Ace Spades, Card Ace Clubs]
    let boardCards = [Card Ace Diamonds]
    let sdeck0 = removeFromDeck boardCards $ sdeck
    let sdeck1 = removeFromDeck myCards $ sdeck0
    putStrLn "\nI take my cards"
    print $ myCards
    putStrLn "\nNew state of deck"
    print $ sdeck1
    putStrLn "\nGiving players their cards"
    let p = giveCardsToPlayers (nPlayers-1) 2 sdeck1
    print $ p
    let sdeck2 = drop ((nPlayers-1)*2) sdeck1
    putStrLn "\nNew state of deck"
    print $ sdeck2
    putStrLn "\ncommunity cards"
    let communityCards = take (5-length boardCards) sdeck2
    print communityCards
    let sdeck3 = drop 5 sdeck2
    putStrLn "\nNew state of deck"
    print $ sdeck3

    print $ reverse $ sort (communityCards++myCards)
    print $ isFlush communityCards myCards

    print "---"

    let communityCardsT = [Card Nine Spades, Card Jack Diamonds, Card Ten Diamonds, Card King Spades, Card Ten Hearts, Card Queen Diamonds, Card Six Spades]
    print $ reverse $ sort (communityCardsT++myCards)
    print $ isStraight 5 communityCardsT myCards
    print $ isFlushRoyal communityCardsT myCards

    print "---"

    let cc = [Card Two Spades, Card Three Spades, Card Four Hearts, Card Five Clubs, Card Ten Spades]
    print $ reverse $ sort (cc++myCards)
    print $ isStraight 5 cc myCards

    print "==="
    print $ isStraight 5 (take 5 sdeck) (drop 5 sdeck)

    print "=============="
    print $ pairsThreesFours communityCardsT myCards
    print $ fullHouse (Card Ace Spades : communityCardsT) myCards
    print $ fullHouse (Card Five Spades : cc) myCards
