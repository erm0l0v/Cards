{-#LANGUAGE TemplateHaskell #-}

import ArbitraryCards

import Game.Cards.CardValue
import Game.Cards.CardSuit
import Game.Cards.Card
import Game.Cards.Hand
import Game.Cards.Combination

import Test.QuickCheck
import Test.QuickCheck.All

prop_first_test :: Int -> Int -> Bool
prop_first_test x y = x + y == y + x

prop_show_card_value :: CardValue -> Bool
prop_show_card_value x = read (show x) == x

prop_show_card_suit :: CardSuit -> Bool
prop_show_card_suit x = read (show x) == x

prop_show_card :: CardValue -> CardSuit -> Bool
prop_show_card v s = read (show x) == x
  where
    x = Card v s

type TestCard = (CardValue, CardSuit)

prop_card_compare :: TestCard -> TestCard -> Bool
prop_card_compare (v1, s1) (v2, s2) = compareValues == compareCards
  where
    compareValues = v1 `compare` v2
    compareCards = Card v1 s1 `compare` Card v2 s2

createTestCard :: TestCard -> Card
createTestCard (v, s) = Card v s

type TestHand = (TestCard, TestCard, TestCard, TestCard, TestCard)

createTestHand :: TestHand -> Hand
createTestHand (tc1, tc2, tc3, tc4, tc5) = fromList xtc
  where
    xtc = map createTestCard [tc1, tc2, tc3, tc4, tc5]

prop_show_hand :: TestHand -> Bool
prop_show_hand th = read (show h) == h
  where
    h = createTestHand th

prop_converting_hand_to_list :: TestHand -> Bool
prop_converting_hand_to_list th = fromList (toList h) == h
  where
    h = createTestHand th

prop_hand_equal_order_undepend :: TestHand -> Bool
prop_hand_equal_order_undepend th = fromList (reverse (toList h)) == h
  where
    h = createTestHand th

return []
main = $quickCheckAll
