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

module Main where

import PokerC
import Data.Semigroup ((<>))
import Options.Applicative

data Opts = Opts
    { argNplayers  :: Int
    , argMyCards   :: String
    , optBoard     :: String
    , optOppRanges :: String
    , optDeadCards :: String
    }

main :: IO ()
main = do
    opts <- execParser optsParser
    let nPlayers          = argNplayers opts
        myCards           = stringToCards (argMyCards opts)
        commCardsKnown    = stringToCards (optBoard opts)
        othersCardsRanges = []
    putStrLn (concat
        [ "Hello, your cards are ", concatMap show myCards
        , ", the board is ", concatMap show commCardsKnown
        , ", your opponents have ", optOppRanges opts
        ])
    pWin <- pWinMonteCarlo 30000 nPlayers myCards commCardsKnown othersCardsRanges
    putStrLn "P(win) = "
    print pWin
  where
    optsParser =
        info
            (helper <*> versionOption <*> programOptions)
            (fullDesc <> progDesc "optparse example" <>
             header
                 "optparse-example - a small example program for optparse-applicative")
    versionOption = infoOption "0.0" (long "version" <> help "Show version")
    programOptions = Opts
        <$> argument auto
            ( metavar "NUMBER_OF_PLAYERS" )
        <*> argument str--auto
            ( metavar "YOUR_CARDS"
           <> help    "E.g. AsAd, Th9h" )
        <*> strOption--option auto
            ( long    "board"
           <> short   'b'
           <> value   ""
           <> help    "Known community cards. E.g.: As9dTh, 2d5s5cJh, QsQhQdQc9h" )
        <*> strOption
            ( long    "opr"
           <> short   'r'
           <> metavar "OPPONENT1RANGE,OPPONENT2RANGE,..."
           <> value   ""
           <> help    "Opponent ranges" )
        <*> strOption
            ( long    "dead"
           <> metavar "DEADCARDS"
           <> value   ""
           <> help    "Cards that are absent from the deck" )



--main :: IO ()
--main = test

