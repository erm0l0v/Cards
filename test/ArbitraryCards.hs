module ArbitraryCards  where
import Game.Cards.CardSuit
import Game.Cards.CardValue
import Game.Cards.Card

import Test.QuickCheck

instance Arbitrary CardSuit where
  arbitrary = elements [Spades, Clubs, Hearts, Diamonds]

instance Arbitrary CardValue where
  arbitrary = elements [Two .. Ace]
