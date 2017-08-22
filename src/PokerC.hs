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
import Data.List (sort, sortBy, groupBy)
import Data.Ord (comparing)
import Data.Maybe-- (listToMaybe, Maybe)

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
    deriving (Eq, Ord, Bounded, Enum)
instance Show Suit where
    show x = case x of
               Clubs    -> "♣"
               Diamonds -> "♦"
               Hearts   -> "♥"
               Spades   -> "♠"

data Card = Card
    { rank :: Rank
    , suit :: Suit
    } deriving Eq
instance Ord Card where
    compare = compare `on` rank
instance Show Card where
    show (Card r s) = show r ++ show s ++ " "

data BestHand =
    HighCard
      { cardHC  :: Rank
      , kickersHC :: [Rank]
      }
  | Pair
      { pairP  :: Rank
      , kickersP :: [Rank]
      }
  | TwoPair
      { pairsTP  :: (Rank, Rank)
      , kickerTP :: Rank
      }
  | ThreeOfAKind Rank
  | Straight Rank
  | Flush Rank
  | FullHouse
      { threeFH :: Rank
      , pairFH  :: Rank
      }
  | FourOfAKind Rank
  | StraightFlush Rank
  | FlushRoyal Bool
      deriving (Show, Ord, Eq)

toFullHouse :: ([Card], [Card]) -> BestHand
toFullHouse highestFullHouse = FullHouse
    { threeFH = (rank . head . fst) highestFullHouse
    , pairFH  = (rank . head . snd) highestFullHouse
    }
toTwoPair :: [[Card]] -> [Card] -> BestHand
toTwoPair highestTwoPair cardsS = TwoPair
    { pairsTP  = to2tuple (map (rank . head) highestTwoPair)
    , kickerTP = rank . head $ removeCards (concat highestTwoPair) cardsS
    }
toPair hP cardsS = Pair
    { pairP    = (rank . head) hP
    , kickersP = map rank $ take 3 $ removeCards hP cardsS
    }
toHighCard cardsS = HighCard
    { cardHC    = rank highestCard
    , kickersHC = map rank $ take 4 $ removeCards [highestCard] cardsS
    } where highestCard = head cardsS

maxHead :: Ord a => Maybe [[a]] -> Maybe [a]
maxHead Nothing = Nothing
maxHead (Just x) = Just $ head . sortBy (flip compare `on` head) $ x--sortByHead

to2tuple :: [a] -> (a, a)
to2tuple [x, y] = (x, y)
to2tuple _ = error "List should be [x, y]"

maybeNotNull :: [a] -> Maybe [a]
maybeNotNull x
  | null x    = Nothing
  | otherwise = Just x

notNullToList :: Maybe [a] -> [a]
notNullToList (Just a) = a
notNullToList Nothing  = []

allSuits = [(minBound :: Suit)..]
allRanks = [(minBound :: Rank)..]

deck = concatMap (\s -> map (flip Card s) allRanks) allSuits

removeCards cards = filter (`notElem` cards)

giveCardsToPlayers _ _ [] = []
giveCardsToPlayers 0 _ _  = []
giveCardsToPlayers nPlayers nCards deck = 
    take nCards deck : giveCardsToPlayers (nPlayers-1) nCards (drop nCards deck)

{-
countSuits cards = map (\s -> length $ filter ((==s) . suit) $ cards) allSuits

isFlushC cards = any (>=5) $ countSuits cards
isFlush comm hand = isFlushC (hand++comm)
-}
findFlushesN :: Int -> [Card] -> Maybe [[Card]]
findFlushesN n = maybeNotNull . filter ((>=n) . length) . groupBy ((==) `on` suit) . sortBy (compare `on` suit) 
findFlushes = findFlushesN 5

predC x | r == Two  = Ace
       | otherwise = pred r
     where r = rank x

findHighestStraightN :: Int -> [Card] -> Maybe [Card]
findHighestStraightN n cards = maybeNotNull $ reverse $ searchS 1 [firstCard] firstCard otherCards
    where firstCard      = head cardsProcessed
          otherCards     = tail cardsProcessed
          cardsProcessed = cardsRevSorted ++ filterA Ace cardsRevSorted
          cardsRevSorted = sortBy (flip compare) cards
          filterA a = filter (\x -> rank x == a)
          searchS i res _ []
            | i == n    = res
            | otherwise = []
          searchS i res prev (x:xs)
            | i <  n && sequentialRank = searchS (i+1) (x:res) x xs
            | sameRank                 = searchS i (x:res) x xs
            | i >= n && not sameRank   = res
            | otherwise                = searchS 1 [x] x xs
            where sameRank       = rank prev  == rank x
                  sequentialRank = predC prev == rank x
findHighestStraight = findHighestStraightN 5

findHighestStraightNinFlushes :: Int -> [[Card]] -> Maybe [Card]
findHighestStraightNinFlushes n flushes
  | res /= [[]] = listToMaybe res
  | otherwise   = Nothing
  where res = (sort . map (notNullToList . findHighestStraightN n)) flushes
findHighestStraightFlushN n = findHighestStraightNinFlushes n . (notNullToList . findFlushesN n)
findHighestStraightFlush = findHighestStraightFlushN 5

isFlushRoyalN :: Int -> [Card] -> Bool
isFlushRoyalN n cards
  | isJust fSF = (== Ace) . rank . head $ fromJust fSF
  | otherwise  = False
  where fSF =  findHighestStraightFlushN n cards
isFlushRoyal = isFlushRoyalN 5

pairsThreesFours :: [Card] -> Maybe [[Card]]
pairsThreesFours cards = maybeNotNull $ sortBy (flip (comparing length)) $ splitRank cardsSorted
    where cardsSorted = sort cards
          splitRank (x:xs) = (x : takeWhile (sameRankAs x) xs) : splitRank (dropWhile (sameRankAs x) xs)
          splitRank []     = []
          sameRankAs x     = (== rank x) .rank 

fullHouse :: [Card] -> Maybe [([Card],[Card])]
fullHouse cards = fullHousePTF $ pairsThreesFours cards
--fullHouse = fullHousePTF . pairsThreesFours

fullHousePTF :: Maybe [[Card]] -> Maybe [([Card],[Card])]
--fullHousePTF ptf = foldl (++) [] $ map (\p -> map (\t -> (t,p)) threes) pairs
fullHousePTF Nothing = Nothing
fullHousePTF (Just ptf) = fullHousePTFj ptf

fullHousePTFj :: [[Card]] -> Maybe [([Card],[Card])]
fullHousePTFj ptf
  | all (not . null) [threes, pairs] = Just [(t,p) | t <- threes, p <- pairs]
  | otherwise                        = Nothing
  where threes = filter ((==3) . length) ptf
        pairs  = filter ((==2) . length) ptf


{-
sortByHead :: Ord a => [[a]] -> [[a]]
sortByHead = sortBy (compare `on` head)
-}

-- Take care with everything that uses maxHead nad assumes head == highest rank

findBestHand :: [Card] -> BestHand
findBestHand cards
  | foundFlushRoyal    = FlushRoyal foundFlushRoyal
  | foundStraightFlush = StraightFlush (rank . head .fromJust $ highestStaightFlush)
  | foundFour          = FourOfAKind (rank . head . fromJust $ highestFour)
  | foundFullHouse     = toFullHouse (fromJust $ highestFullHouse fh)
  | foundFlush         = Flush (rank . head $ fromJust highestFlush)
  | foundStraight      = Straight (rank . head $ fromJust highestStaight)
  | foundThree         = ThreeOfAKind (rank . head $ fromJust highestThree)
  | foundTwoPair       = toTwoPair highestTwoPair cardsS
  | foundPair          = toPair (fromJust highestPair) cardsS
  | otherwise          = toHighCard cardsS
  where
      cardsS              = sortBy (flip compare) cards

      foundFlushRoyal     = isFlushRoyal cardsS
      foundStraightFlush  = isJust highestStaightFlush
      foundFour           = isJust fours
      foundFullHouse      = isJust fh
      foundFlush          = isJust highestFlush
      foundStraight       = isJust highestStaight
      foundThree          = isJust highestThree
      foundTwoPair | isJust pairs = length (fromJust pairs) >= 2
                   | otherwise    = False
      foundPair    | isJust pairs = length (fromJust pairs) == 1
                   | otherwise    = False

      highestStaightFlush = findHighestStraightFlush cardsS
      highestFour         = maxHead fours
      highestFullHouse Nothing   = Nothing
      highestFullHouse (Just fh) = Just $ head $ sortBy (flip compare `on` (head . fst)) fh
      highestFlush        = maxHead flushes
      highestStaight      = findHighestStraight cardsS
      highestThree        = maxHead threes
      highestTwoPair      = take 2 $ sortBy (flip compare `on` head) (fromJust pairs)
      highestPair         = maxHead pairs

      flushes = findFlushes cardsS
      ns :: Int -> Maybe [[Card]] -> Maybe [[Card]]
      ns n (Just ptf) = maybeNotNull $ filter ((==n) . length) ptf
      ns _ Nothing    = Nothing
      fours  = ns 4 ptf
      threes = ns 3 ptf
      pairs  = ns 2 ptf
      fh      = fullHousePTF ptf
      ptf     = pairsThreesFours cardsS

{-
findBestHand :: [Card] -> BestHand
findBestHand cards
  | isFlushRoyal     = FlushRoyal isStraightFlush
  | isStraightFlush  = StraightFlush (head highestStaightFlush)
  | isFour           = FourOfAKind (head highestFour)
  | isFullHouse      = toFullHouse $ highestFullHouse
  | isFlush          = highestFlush
  | isStraight       = highestStaight
  | isThree          = highestThree
  | isTwoPair        = (highestTwoPair, kickerTP)
  | isPair           = (highestPair, kickersP)
  | otherwise        = (highCard, kickersHC)
  where n = 5
        cardsS = sort cards
        isFlushRoyal = (== Ace) . rank . head $ highestStaightFlush
        isStraightFlush = highestStaightFlush /= []
        isFour = fours /= []
        isFullHouse = fh /= []
        isFlush = flushes /=[]
        isStraight = highestStaight /= []
        isThree = threes /= []
        isTwoPair = length pairs >= 2
        isPair = pairs /= []
        highestStaightFlush = findHighestStraightsNflushes n flushes
        highestFour = (head . sort . map head) fours
        highestFullHouse = head $ sortBy (compare `on` (head . fst)) fh
        toFullHouse hf = FullHouse {threeFH=(head . fst) hf, pairFH=(head . snd) hf}
        highestFlush = (head . sort . map head) flushes
        highestStaight = findHighestStraightN n cards
        highestThree = (head . sort . map head) threes
        highestPair = (head . sort . map head) pairs
        highestTwoPair = take 2 $ sortBy (compare `on` (rank . head)) pairs
        highCard = head cardsS
        kickerTP = take 1 $ removeCards (concat highestTwoPair) cardsS
        kickersP = (take 3 . removeCards [highestPair]) cardsS
        kickersHC = (take 3 . removeCards [highCard]) cardsS
        flushes = findFlushesN n cards
        fours = filter ((==4) . length) ptf
        threes = filter ((==3) . length) ptf
        pairs = filter ((==2) . length) ptf
        fh = fullHousePTF ptf
        ptf = pairsThreesFours cards
-}

testForSdeck [] = []
testForSdeck sdeck = (tn, findBestHand tn) : testForSdeck dn
    where tn = take 5 sdeck
          dn = drop 5 sdeck

test = do
    let cards = 
            [ Card Three Diamonds
            , Card Five Diamonds
            , Card Eight Spades
            , Card Nine Diamonds
            , Card King Diamonds
            , Card King Hearts
            , Card Six Diamonds
            ]
        cardsS = sortBy (flip compare) cards
    print $ findBestHand cards

test1 = do
    sdeck <- shuffleM deck
    putStrLn "Initial state of deck"
    print $ sdeck

    let nPlayers = 6
    let myCards = [Card Ace Spades, Card Ace Clubs]
    let boardCards = [Card Ace Diamonds]
    let sdeck0 = removeCards boardCards sdeck
    let sdeck1 = removeCards myCards sdeck0
    putStrLn "\nI take my cards"
    print myCards
    putStrLn "\nNew state of deck"
    print sdeck1
    putStrLn "\nGiving players their cards"
    let p = giveCardsToPlayers (nPlayers-1) 2 sdeck1
    print p
    let sdeck2 = drop ((nPlayers-1)*2) sdeck1
    putStrLn "\nNew state of deck"
    print sdeck2
    putStrLn "\ncommunity cards"
    let communityCards = take (5-length boardCards) sdeck2
    print communityCards
    let sdeck3 = drop 5 sdeck2
    putStrLn "\nNew state of deck"
    print sdeck3

    print $ sortBy (flip compare) (communityCards ++ myCards)
    print $ findFlushes (communityCards++myCards)

    print "-look for Straight and FLush Royal--"

    let communityCardsT = [Card Nine Spades, Card Jack Diamonds, Card Ten Diamonds, Card King Spades, Card Ten Hearts, Card Queen Diamonds, Card Six Spades]
    print $ sortBy (flip compare) (communityCardsT++myCards)
    print $ findHighestStraight $ communityCardsT ++ myCards
    print $ isFlushRoyal $ communityCardsT ++ myCards
    print $ findHighestStraight $ deck
    print $ isFlushRoyal $ deck

    print "---"

    let cc = [Card Two Spades, Card Three Spades, Card Four Hearts, Card Five Clubs, Card Ten Spades]
    print $ sortBy (flip compare) (cc++myCards)
    --print $ isStraight 5 cc myCards

    print "==="
    --print $ isStraight 5 (take 5 sdeck) (drop 5 sdeck)

    print "=============="
    print $ pairsThreesFours (communityCardsT++myCards)
    print $ fullHouse $ (Card Ace Spades : communityCardsT)++myCards
    --print $ fullHouse (Card Five Spades : cc) myCards
    print "LOOK FOR ALL"
    --print $ findBestHand (communityCards++myCards)
    
    print $ findHighestStraightN 5 $ take 5 sdeck3
    print $ findHighestStraightN 5 $ [Card Ten Spades, Card Jack Diamonds, Card Nine Spades, Card Eight Clubs, Card Queen Hearts]
    mapM_ print $ testForSdeck sdeck3
    print "testing Hand Vs Hand"
    print $ length sdeck
    let ha = take 7 sdeck
        hb = take 7 . drop 7 $ sdeck
    putStrLn $ "HandA " ++ show ha
    putStrLn $ "HandB " ++ show hb
    let handa = findBestHand ha
    let handb = findBestHand hb
    putStrLn "   "
    putStrLn $ "HandA " ++ show handa
    putStrLn $ "HandB " ++ show handb
    putStrLn "   "
    putStrLn $ "HandA vs HandB " ++ show (compare handa handb)
