module Game.Cards.Hand
(
  Hand(..),
  toList,
  fromList
) where

import Game.Cards.Card
import Data.List

data Hand = Hand Card Card Card Card Card

toList :: Hand -> [Card]
toList (Hand c1 c2 c3 c4 c5) = [c1, c2, c3, c4, c5]

fromList :: [Card] -> Hand
fromList [c1, c2, c3, c4, c5] = Hand c1 c2 c3 c4 c5
fromList _ = error "List shoul contains 5 elements"

join :: (Show a) => String -> [a] -> String
join _ [] = ""
join _ [x] = show x
join s (x:xs) = show x ++ s ++ join s xs

instance Show Hand where
  show h = join " " $ toList h

instance Read Hand where
  readsPrec _ xs =  [(fromList $ map read $ words xs, "")]

instance Eq Hand where
  h1 == h2 = compareLists lh1 lh2
    where
      lh1 = sort $ toList h1
      lh2 = sort $ toList h2
      compareLists :: [Card] -> [Card] -> Bool
      compareLists [] _ = True
      compareLists (x:xs) hs = x `elem` hs && compareLists xs hs
