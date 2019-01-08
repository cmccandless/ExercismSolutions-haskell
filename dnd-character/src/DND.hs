module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Test.QuickCheck (Gen, choose, vectorOf)

data Character = Character
  { name         :: String
  , strength     :: Int
  , dexterity    :: Int
  , constitution :: Int
  , intelligence :: Int
  , wisdom       :: Int
  , charisma     :: Int
  , hitpoints    :: Int
  }
  deriving (Show, Eq)

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = sort small ++ (x : sort large)
  where small = [y | y <- xs, y <= x]
        large = [y | y <- xs, y > x]

modifier :: Int -> Int
modifier = flip div 2 . flip (-) 10

dice :: Gen Int
dice = choose (1, 6)

ability :: Gen Int
ability = do
  ds <- vectorOf 4 dice
  return $ sum . tail $ sort ds

character :: Gen Character
character = do
  st <- ability
  dx <- ability
  cns <- ability
  intl <- ability
  wis <- ability
  chrs <- ability
  return $ Character "Adventurer" st dx cns intl wis chrs ((+) 10 $ modifier cns)
