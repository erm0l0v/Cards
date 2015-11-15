module Game.Cards.CardSuit
(
  CardSuit(..)
) where

data CardSuit = Spades | Clubs | Hearts | Diamonds
              deriving (Eq)

instance Show CardSuit where
  show Spades = "S"
  show Clubs = "C"
  show Hearts = "H"
  show Diamonds = "D"

instance Read CardSuit where
  readsPrec _ ('S':xs) = [(Spades, xs)]
  readsPrec _ ('C':xs) = [(Clubs, xs)]
  readsPrec _ ('H':xs) = [(Hearts, xs)]
  readsPrec _ ('D':xs) = [(Diamonds, xs)]
  readsPrec _ _ = error "Can't read this string"
