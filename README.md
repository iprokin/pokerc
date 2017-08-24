# pokerc

Functional Texas Hold'em Poker Odds Calculator.


The goal of this project is to learn Haskell and develop a solid cross-platform alternative to PokerStove.


What works:
* Finding out combinations.
* Determination of the winner
* Monte-Carlo computation of P(win).
* Basic commandline interface

To-to:
* Add kickers to FourOfAKind and ThreeOfAKind datatypes.
* Commandline user interface
    - Ranges for opponents
* Clean up code
* Optimize computations

## Install

Compile:

`stack build`
Collect executable <https://stackoverflow.com/questions/31466539/where-does-stack-build-store-generated-executables>.

# Usage examples

`pokerc` computes your probability of winning by simulating many random games with the same constraints (specified by commandline options).

See all available options with:

`pokerc --help`

At least, you need to specify the number of opponents and your cards.

For example, you have Ace Spades and Ace Diamonds and you are playing pre-flop against 5 opponents (6 players total):

`pokerc 6 AsAd`

You are playing the same game after seeing flop (2d Kh Ks) and no one folded:

`pokerc 6 AsAd --board 2dKhKs`

One opponent folded, you see Kd on the turn:
`pokerc 5 AsAd --board 2dKhKsKd`

River is Kc, 3 more opponents folded:
`pokerc 2 AsAd --board 2dKhKsKd`

# References

* https://hackage.haskell.org/package/poker-eval
* http://pokerini.com/help/holdem_range_notation.php
* https://haskell-lang.org/library/optparse-applicative
* http://poker.readthedocs.io/en/latest/range.html

<pre>
Copyright 2017 Ilya Prokin

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
</pre>
