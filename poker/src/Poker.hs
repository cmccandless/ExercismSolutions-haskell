module Poker (bestHands) where

import Data.Char (isDigit)
import Data.List (nub, sortBy, maximumBy, partition)

types :: [String]
types = [ "11111"
        , "2111"
        , "221"
        , "311"
        , "S"
        , "F"
        , "32"
        , "41"
        , "SF"
        ]

compareBy :: Ord b => (a -> b) -> a -> a -> Ordering
compareBy f x y = compare (f x) (f y)

groupby :: Eq b => (a -> b) -> [a] -> [[a]]
groupby _ [] = []
groupby keyFunc (x:xs) = let groupKey = (==) (keyFunc x) . keyFunc
                             (left, right) = partition groupKey xs
                         in (x : left) : groupby keyFunc right

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn delim s = first : splitOn delim second
    where 
        first = takeWhile (delim /=) s
        second = dropWhile (delim ==) $ dropWhile (delim /=) s

cardValue :: String -> Int
cardValue card =
    let value = init card
    in case (all isDigit value, value) of
        (True, _) -> read value :: Int
        (_, "J") -> 11
        (_, "Q") -> 12
        (_, "K") -> 13
        (_, "A") -> 14
        _        -> -1

isStraight :: [Int] -> Bool
isStraight values = values == [14, 5, 4, 3, 2] ||
    length values == 5 && values == straight
    where
        maxValue = maximum values
        straight = [maxValue, (maxValue - 1)..(maxValue - 4)]

indexOf :: Eq a => a -> [a] -> Int
indexOf _ [] = -1
indexOf x (y:ys) = 
    let i = indexOf x ys
    in if x == y
       then 0
       else if i == -1
       then -1
       else i + 1

numFmt :: Int -> String
numFmt n = if n < 10
           then "0" ++ show n
           else show n

compareValueGroup :: Ord a => [a] -> [a] -> Ordering
compareValueGroup x y = let gcmp = compareBy length x y
                        in case gcmp of
    EQ -> compareBy head x y
    _  -> gcmp

-- Produces a unique key for a hand, where the number before
-- the underscore is the index in `types`, and the number after
-- represents the unique card values in the hand,
-- sorted by the count of that card value within the hand, descending,
-- then by the card value, descending.
-- This allows for a lexical sorting of hands.
-- Examples:
--   "4S 5H 6C 8D KH" -> "0_1308060504"
handKey :: String -> String
handKey handStr = let hand = splitOn ' ' handStr
                      flush = (==) 1 . length . nub $ map last hand
                      cardValues = map cardValue hand
                      cardsByValueGroupedDesc
                        = sortBy (flip compareValueGroup)
                          $ groupby id cardValues
                      cardsByValueKeys = map head cardsByValueGroupedDesc
                      cardCountsDesc
                        = sortBy (flip compare)
                          $ map length cardsByValueGroupedDesc
                      values = nub cardsByValueKeys
                      straight = isStraight values
                      countsStr = concatMap show cardCountsDesc
                      _class = if not (straight || flush)
                               then indexOf countsStr types
                               else let typeStr
                                            = case (straight, flush) of
                                            (True, True) -> "SF"
                                            (True, False) -> "S"
                                            (False, True) -> "F"
                                            _ -> ""
                                    in indexOf typeStr types
                      valuesStr = if values == [14, 5, 4, 3, 2]
                                  then map numFmt [5, 4..1]
                                  else map numFmt cardsByValueKeys
                  in show _class ++ "_" ++ concat valuesStr

compareHandGroupings :: [String] -> [String] -> Ordering
compareHandGroupings (x:_) (y:_) = compareBy handKey x y
compareHandGroupings _ _ = LT

validCard :: String -> Bool
validCard card = let suit = last card
                     value = cardValue card
                 in suit `elem` "CDHS" && value `elem` [2..14]

validHand :: [String] -> Bool
validHand cards = length cards == 5 && all validCard cards

bestHands :: [String] -> Maybe [String]
bestHands xs
    | not (null xs) && all (validHand . splitOn ' ') xs
        = Just . maximumBy compareHandGroupings
        $ groupby handKey xs
    | otherwise = Nothing
