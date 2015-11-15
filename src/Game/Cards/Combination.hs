module Game.Cards.Combination (
  HandRank,
  CombinationInfo,
  getCombination
) where
import Game.Cards.CardValue
import Game.Cards.Card
import Game.Cards.Hand
import Data.List
import Control.Applicative

data HandRank = HighCard | OnePair | TwoPairs | ThreeOfAKind |
                Straight | Flush | FullHouse | FourOfAKind |
                StraightFlush | RoyalFlush
              deriving (Eq, Ord, Show, Enum)

type Kicker = [CardValue]
type RestCards = [Card]
type CombinationInfo = (HandRank, Kicker, RestCards)


cardCompare :: Card -> Card -> Ordering
cardCompare a b = getValue a `compare` getValue b

sortCardsDes :: [Card] -> [Card]
sortCardsDes = sortBy (flip cardCompare)

sortCards :: [Card] -> [Card]
sortCards = sortBy cardCompare

pair :: [Card] -> Maybe (CardValue, RestCards)
pair cards = getPair (sortCardsDes cards) []
  where
    getPair [] _ = Nothing
    getPair [c] _ = Nothing
    getPair (c1:c2:cs) rest | cardCompare c1 c2 == EQ = Just (getValue c1, rest ++ cs)
                            | otherwise = getPair (c2:cs) (rest ++ [c1])

three :: [Card] -> Maybe (CardValue, RestCards)
three cards = getThree (sortCardsDes cards) []
  where
    getThree [] _ = Nothing
    getThree [c] _ = Nothing
    getThree [c1, c2] _ = Nothing
    getThree (c1:c2:c3:cs) rest | cardCompare c1 c2 == EQ && cardCompare c2 c3 == EQ = Just (getValue c1, rest ++ cs)
                                | cardCompare c2 c3 /= EQ = getThree (c3:cs) (rest ++ [c1, c2])
                                | otherwise = getThree (c2:c3:cs) (rest ++ [c1])


ordered :: [Card] -> Bool
ordered cards = isOrderes c cs
  where
    (c:cs) = map getValue $ sortCards cards
    isOrderes _ [] = True
    isOrderes current (v:vs) | v == next current = isOrderes v vs
                             | otherwise = False

-------------------------------------------------------------------------------

checkRankInfo :: HandRank -> [Card] -> Maybe CombinationInfo
checkRankInfo _ [] = Nothing

checkRankInfo HighCard cards = Just (HighCard, [getValue c], cs)
  where
    c:cs = sortCardsDes cards

checkRankInfo OnePair cards = infoConvert <$> pair cards
  where
    infoConvert (v, cs) = (OnePair, [v], cs)

checkRankInfo TwoPairs cards = do
  (_, k1, restCards) <- checkRankInfo OnePair cards
  (_, k2, lastCard) <- checkRankInfo OnePair restCards
  return (getResult (k1 ++ k2) lastCard)
  where
    getResult kicker lastCard = (TwoPairs, kicker, lastCard)

checkRankInfo ThreeOfAKind cards = infoConvert <$> three cards
  where
    infoConvert (v, cs) = (ThreeOfAKind, [v], cs)

checkRankInfo Straight cards | ordered cards = do
                              (_, [maxCard], _) <- checkRankInfo HighCard cards
                              return (Straight, [maxCard], [])
                           | otherwise = do
                             (_, [maxCard], rest) <- checkRankInfo HighCard cards
                             (_, [kicker], _) <- checkRankInfo HighCard rest
                             if ordered rest && maxCard == Ace && kicker == Four
                               then
                                 return (Straight, [kicker], [])
                               else
                                 Nothing

checkRankInfo Flush cards = if allValuesSame cards
                            then
                              infoConvert <$> checkRankInfo HighCard cards
                            else
                              Nothing
  where
    infoConvert (_, value, _) = (Flush, value, [])
    allValuesSame [c] = True
    allValuesSame (c1:c2:cs) = (getSuit c1 == getSuit c2) && allValuesSame (c2:cs)

checkRankInfo FullHouse cards = do
  (_, [threeValue], rest) <- checkRankInfo ThreeOfAKind cards
  (_, [pairValue], _) <- checkRankInfo OnePair rest
  return (FullHouse, [threeValue, pairValue], [])

checkRankInfo FourOfAKind cards = do
  (_, [value1], rest) <- checkRankInfo OnePair cards
  (_, [value2], last) <- checkRankInfo OnePair rest
  if value1 == value2
    then
      return (FourOfAKind, [value1], last)
    else
      Nothing

checkRankInfo StraightFlush cards = do
  (_, value, _) <- checkRankInfo Straight cards
  _ <- checkRankInfo Flush cards
  return (StraightFlush, value, [])

checkRankInfo RoyalFlush cards = do
  (_, [value], _) <- checkRankInfo StraightFlush cards
  if value == Ace
    then
      return (RoyalFlush, [value], [])
    else
      Nothing

-------------------------------------------------------------------------------

firstValue :: [Maybe a] -> a
firstValue (Nothing:ms) = firstValue ms
firstValue (Just a:_) = a

getCombination :: Hand -> CombinationInfo
getCombination h = firstValue values
  where
    cards = toList h
    ranks = reverse [HighCard .. RoyalFlush]
    values = map (`checkRankInfo` cards) ranks
