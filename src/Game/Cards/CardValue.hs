module Game.Cards.CardValue
(
  CardValue(..),
  next
) where

data CardValue = Two | Three | Four | Five | Six | Seven |
                 Eight | Nine | Ten | Jack | Queen | King | Ace
               deriving (Eq, Ord, Enum, Bounded)

instance Show CardValue where
  show Two = "2"
  show Three = "3"
  show Four = "4"
  show Five = "5"
  show Six = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine = "9"
  show Ten = "T"
  show Jack = "J"
  show Queen = "Q"
  show King = "K"
  show Ace = "A"

instance Read CardValue where
  readsPrec _ ('2':xs) = [(Two, xs)]
  readsPrec _ ('3':xs) = [(Three, xs)]
  readsPrec _ ('4':xs) = [(Four, xs)]
  readsPrec _ ('5':xs) = [(Five, xs)]
  readsPrec _ ('6':xs) = [(Six, xs)]
  readsPrec _ ('7':xs) = [(Seven, xs)]
  readsPrec _ ('8':xs) = [(Eight, xs)]
  readsPrec _ ('9':xs) = [(Nine, xs)]
  readsPrec _ ('T':xs) = [(Ten, xs)]
  readsPrec _ ('J':xs) = [(Jack, xs)]
  readsPrec _ ('Q':xs) = [(Queen, xs)]
  readsPrec _ ('K':xs) = [(King, xs)]
  readsPrec _ ('A':xs) = [(Ace, xs)]
  readsPrec _ _ = error "Can't read this string"


next :: CardValue -> CardValue
next v | v == maxBound = minBound
       | otherwise = succ v
