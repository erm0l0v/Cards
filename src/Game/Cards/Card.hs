module Game.Cards.Card
(
  Card(..),
  getValue,
  getSuit
) where

import Game.Cards.CardSuit
import Game.Cards.CardValue


data Card = Card CardValue CardSuit
  deriving (Eq)

instance Ord Card where
  Card v1 _ `compare` Card v2 _ = v1 `compare` v2

instance Show Card where
  show (Card v s) = show v ++ show s

instance Read Card where
  readsPrec _ (a:b:xs) = [(Card (read [a]) (read [b]), xs)]
  readsPrec _ _ = error "Can't read this string"

getValue :: Card -> CardValue
getValue (Card v _) = v

getSuit :: Card -> CardSuit
getSuit (Card _ s) = s
