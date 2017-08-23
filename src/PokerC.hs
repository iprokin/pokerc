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
import Control.Monad.Random
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
  | FullHouse Rank
  | FourOfAKind Rank
  | StraightFlush Rank
  | FlushRoyal
      deriving (Show, Ord, Eq)

sortR :: Ord a => [a] -> [a]
sortR = sortBy (flip compare)

safeFilter ::(a -> Bool) -> [a] -> Maybe [a]
safeFilter f = maybeNotNull . filter f

--filterNsPTF n ptf = onJust (safeFilter ((>=n) . length)) ptf
filterNsPTF :: Foldable t => (Int -> Bool) -> Maybe [t a] -> Maybe [t a]
filterNsPTF f Nothing = Nothing
filterNsPTF f (Just ptf)
  | (not.null) frd = Just frd
  | otherwise      = Nothing
  where frd = filter (f . length) ptf

bestFlushRoyal :: [Card] -> Maybe BestHand
bestFlushRoyal cards
  | foundFR   = Just FlushRoyal
  | otherwise = Nothing 
  where foundFR = isFlushRoyal cards

bestStraightFlush :: [Card] -> Maybe BestHand
bestStraightFlush cards = onJust (StraightFlush . rank . head) hsf
    where hsf = findHighestStraightFlush cards

bestStraight :: [Card] -> Maybe BestHand
bestStraight cards = onJust (Straight . rank . head) hs
    where hs = findHighestStraight cards

bestXOfAKind :: Int -> (Rank -> BestHand) -> [Card] -> Maybe BestHand
bestXOfAKind n dataC cards = onJust (dataC . takeWithHighestRank) nplets
    where
        takeWithHighestRank = head . sortR . map (rank . head)
        ptf   = pairsThreesFours cards
        nplets = filterNsPTF (>=n) ptf

bestFourOfAKind  = bestXOfAKind 4 FourOfAKind
bestThreeOfAKind = bestXOfAKind 3 ThreeOfAKind

bestHighCard :: [Card] -> Maybe BestHand
bestHighCard []    = Nothing
bestHighCard [x]   = Nothing
bestHighCard cards = Just HighCard
    { cardHC    = head ranksS
    , kickersHC = (take 4 . tail) ranksS 
    } where ranksS = (map rank . sortR) cards

--{-
bestPair :: [Card] -> Maybe BestHand
bestPair cards = onJust (makePair cardsS . takeHighestPair) nplets
    where
        takeHighestPair = head . sortBy (flip compare `on` head)
        makePair cardsS hP = Pair
            { pairP    = (rank . head) hP
            , kickersP = map rank $ take 3 $ removeCards hP cardsS
            }
        ptf    = pairsThreesFours cards
        nplets = filterNsPTF (>=2) ptf
        cardsS = sortR cards

bestTwoPair :: [Card] -> Maybe BestHand
bestTwoPair cards = onJust (makeTwoPair cardsS) (takeHighestTwoPair nplets)
    where
        takeHighestTwoPair (Just nplets)
            | length hTP == 2 = Just hTP
            | otherwise       = Nothing
            where hTP = take 2 . sortBy (flip compare `on` (rank . head)) $ nplets
        takeHighestTwoPair Nothing = Nothing

        makeTwoPair cardsS hTP = TwoPair
            { pairsTP  = to2tuple (map (rank . head) hTP)
            , kickerTP = rank . head $ removeCards (concat hTP) cardsS
            }

        ptf    = pairsThreesFours cards
        nplets = filterNsPTF (>=2) ptf
        cardsS = sortR cards
--}
bestFullHouse :: [Card] -> Maybe BestHand
bestFullHouse cards = onJust (FullHouse . takeHighestFH) fh
    where
        takeHighestFH   = head . sortR . map (rank . head . fst)
        fh              = fullHouse cards

{-
bestOfAKind :: [Card] -> Maybe BestHand
bestOfAKind cards
  | n == Just 4 = onJust (FourOfAKind  . takeH) (sel 4)
  | n == Just 3 = onJust (ThreeOfAKind . takeH) (sel 3)
  | n == Just 2 = dealWithPairs (sel 2)
  | otherwise   = Nothing
    where
        dealWithPairs pairs
          | onJust length pairs >= Just 2 =
              onJust (makeTwoPair cardsS) (takeHighestTwoPair pairs)
          | otherwise                     =
              onJust (makePair cardsS . takeHighestPair) pairs

        takeHighestPair = head . sortBy (flip compare `on` head)
        makePair cardsS hP = Pair
            { pairP    = (rank . head) hP
            , kickersP = map rank $ take 3 $ removeCards hP cardsS
            }

        takeHighestTwoPair (Just nplets)
            | length hTP == 2 = Just hTP
            | otherwise       = Nothing
            where
                hTP = take 2 . sortBy (flip compare `on` (rank . head)) $ nplets
        takeHighestTwoPair Nothing = Nothing

        makeTwoPair cardsS hTP = TwoPair
            { pairsTP  = to2tuple (map (rank . head) hTP)
            , kickerTP = rank . head $ removeCards (concat hTP) cardsS
            }

        takeH   = head . sortR . map (rank . head)
        ptf     = pairsThreesFours cards
        ptfGE2  = filterNsPTF (>=2) ptf
        sel n   = filterNsPTF (==n) ptfGE2
        n       = onJust (maximum . map length) ptfGE2
        cardsS  = sortR cards
--}
bestFlush :: [Card] -> Maybe BestHand
bestFlush cards = onJust (Flush . maximum . map (rank . maximum)) $ findFlushes cards

findBestHand :: [Card] -> BestHand
findBestHand cards = head . catMaybes . map ($ cards) $
          [ bestFlushRoyal
          , bestStraightFlush
          , bestFourOfAKind
          , bestFullHouse
          , bestFlush
          , bestStraight
          , bestThreeOfAKind
          , bestTwoPair
          , bestPair
          , bestHighCard
          ]

maxHead :: Ord a => Maybe [[a]] -> Maybe [a]
maxHead Nothing = Nothing
maxHead (Just x) = listToMaybe . sortBy (flip compare `on` head) $ x--sortByHead

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

onJust :: (a -> b) -> Maybe a -> Maybe b
onJust f (Just x) = Just (f x)
onJust _ Nothing  = Nothing

allSuits = [(minBound :: Suit)..]
allRanks = [(minBound :: Rank)..]

deck = concatMap (\s -> map (flip Card s) allRanks) allSuits

removeCards cards = filter (`notElem` cards)

giveCardsToPlayers _ _ [] = []
giveCardsToPlayers 0 _ _  = []
giveCardsToPlayers nPlayers nCards deck = 
    take nCards deck : giveCardsToPlayers (nPlayers-1) nCards (drop nCards deck)

findFlushesN :: Int -> [Card] -> Maybe [[Card]]
findFlushesN n = maybeNotNull . filter ((>=n) . length) . groupBy ((==) `on` suit) . sortBy (comparing suit)
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
pairsThreesFours = 
    maybeNotNull .
        sortBy (flip (comparing length)) .
            groupBy ((==) `on` rank) .
                sortBy (flip compare `on` rank)
{-
pairsThreesFours cards = maybeNotNull $ sortBy (flip (comparing length)) $ splitRank cardsSorted
    where cardsSorted = sort cards
          splitRank (x:xs) = (x : takeWhile (sameRankAs x) xs) : splitRank (dropWhile (sameRankAs x) xs)
          splitRank []     = []
          sameRankAs x     = (== rank x) .rank 
-}
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

{-
findBestHand :: [Card] -> BestHand
findBestHand cards
  | foundFlushRoyal    = FlushRoyal foundFlushRoyal
  | foundStraightFlush = StraightFlush (rank . head .fromJust $ highestStaightFlush)
  | foundFour          = FourOfAKind (rank . head . fromJust $ highestFour)
  | foundFullHouse     = toFullHouse (highestFullHouse fh)
  | foundFlush         = Flush (rank . head $ fromJust highestFlush)
  | foundStraight      = Straight (rank . head $ fromJust highestStaight)
  | foundThree         = ThreeOfAKind (rank . head $ fromJust highestThree)
  | foundTwoPair       = fromJust hTP
  | foundPair          = toPair highestPair cardsS
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

      hTP = toTwoPair highestTwoPair cardsS

      flushes = findFlushes cardsS
      ns :: Int -> Maybe [[Card]] -> Maybe [[Card]]
      ns n (Just ptf) = maybeNotNull $ filter ((==n) . length) ptf
      ns _ Nothing    = Nothing
      fours  = ns 4 ptf
      threes = ns 3 ptf
      pairs  = ns 2 ptf
      fh      = fullHousePTF ptf
      ptf     = pairsThreesFours cardsS
-}

{-
testForSdeck [] = []
testForSdeck sdeck = (tn, findBestHand tn) : testForSdeck dn
    where tn = take 5 sdeck
          dn = drop 5 sdeck


test1 = do
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
-}

enumerateAndRankPlayers communityCards playersCards = rankedPlayers
    where
        playersRes    = zip playersCards $
            map (findBestHand . (++communityCards)) playersCards
        enumeratedRes = zip [1..length playersRes] playersRes
        rankedPlayers = sortBy cmp enumeratedRes
        cmp = flip compare `on` snd . snd

findWinner :: [Card] -> [[Card]] -> (Int, ([Card], BestHand))
findWinner cC = head . enumerateAndRankPlayers cC

-- Play game and see if I (the player #1) won.
playGame :: [Card] -> Int -> [Card] -> [Card] -> [[Card]] -> Bool
playGame shuffledDeck nPlayers myCards commCardsKnown othersCardsKnown =
    1 == (fst $ findWinner communityCards allPlayersCards)
        where
            knownCards              = myCards ++ commCardsKnown ++ (concat othersCardsKnown)
            deckNoKnownCards        = removeCards knownCards shuffledDeck
            nOtherPlayersGivenCards = length othersCardsKnown
            n                       = nPlayers - nOtherPlayersGivenCards - 1
            othersCardsUnknown      = giveCardsToPlayers n 2 deckNoKnownCards
            deckNoKnownNoPlayers    = drop (2*n) deckNoKnownCards
            commCardsRemaining      = take (5-length commCardsKnown) deckNoKnownNoPlayers
            communityCards          = commCardsKnown ++ commCardsRemaining
            allPlayersCards         = myCards : (othersCardsKnown ++ othersCardsUnknown)

playGameImpure :: MonadRandom m => Int -> [Card] -> [Card] -> [[[Card]]] -> m Bool
playGameImpure nPlayers myCards commCardsKnown othersCardsRanges = do
    sdeck <- shuffleM deck
    let pGI = playGame sdeck nPlayers myCards commCardsKnown
     in
     case othersCardsRanges of
       []        -> do
           oCRshuffled <- mapM shuffleM othersCardsRanges
           let othersCardsKnown = map head oCRshuffled
           return (pGI othersCardsKnown)
       otherwise -> do
           return (pGI [])

{-
-- Heavy on memory, but ok for small nSim
pWinMonteCarlo :: MonadRandom m => Integer -> Int -> [Card] -> [Card] -> [[[Card]]] -> m Double
pWinMonteCarlo nSim nPlayers myCards commCardsKnown othersCardsRanges = do
    let pGI = playGameImpure nPlayers myCards commCardsKnown othersCardsRanges
    nWin <- mapM (\_ -> pGI) [1..nSim] >>= return . length . filter id
    return (fromIntegral nWin / fromIntegral nSim)
--}

{-
foldM' :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ z [] = return z
foldM' f z (x:xs) = do
    z' <- f z x
    z' `seq` foldM' f z' xs

pWinMonteCarlo :: MonadRandom m => Integer -> Int -> [Card] -> [Card] -> [[[Card]]] -> m Double
pWinMonteCarlo nSim nPlayers myCards commCardsKnown othersCardsRanges = do
    let pGI = playGameImpure nPlayers myCards commCardsKnown othersCardsRanges
        f total _ = do
            pR <- pGI
            if pR
               then return total
               else return (total+1)
    nWin <- foldM' f 0 [1..nSim]
    return (fromIntegral nWin / fromIntegral nSim)
--}

--{-
pWinMonteCarlo :: MonadRandom m => Integer -> Int -> [Card] -> [Card] -> [[[Card]]] -> m Double
pWinMonteCarlo nSim nPlayers myCards commCardsKnown othersCardsRanges = do
    let pGI = playGameImpure nPlayers myCards commCardsKnown othersCardsRanges
    let go 0 total = return total
        go n total = do
            pR <- pGI
            if pR 
               then go (n-1) (total+1)
               else go (n-1) total
    nWin <- go nSim 0
    return (fromIntegral nWin / fromIntegral nSim)
--}

--{-
test = do
    let myCards           = [Card Ace Spades, Card Ace Clubs]
        commCardsKnown    = []
        othersCardsRanges = []
    pWin <- pWinMonteCarlo 30000 3 myCards commCardsKnown othersCardsRanges
    print pWin
--}

test0 = do
    sdeck <- shuffleM deck
    putStrLn "Initial state of deck"
    print $ sdeck

    let nPlayers = 6
    let myCards = [Card Ace Spades, Card Ace Clubs]
    let commCardsKnown = [Card Ace Diamonds]
    --let sdeck0 = removeCards commCardsKnown sdeck
    --let sdeck1 = removeCards myCards sdeck0
    let sdeck1 = removeCards (myCards ++ commCardsKnown) sdeck
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
    let commCardsRemaining = take (5-length commCardsKnown) sdeck2
    let communityCards = commCardsKnown ++ commCardsRemaining
    print communityCards
    let sdeck3 = drop 5 sdeck2
    putStrLn "\nNew state of deck"
    print sdeck3

    putStrLn "\nAnd...the winner is...\n"
    print $ findWinner communityCards (myCards : p)
    putStrLn "\n"
    mapM_ print $ tail $ enumerateAndRankPlayers communityCards (myCards : p)
