module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Data.List (permutations)
import Data.Maybe (isNothing, fromJust)

data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Show, Enum)

data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)
                         
perms :: [[Int]]
perms = permutations [0..4]

nextTo :: Int -> Int -> Bool
nextTo a b = abs (a - b) == 1

solve :: Solution
solve = fromJust $ solveResidents perms
    where
        [first,second,middle,_,_] = [0..4]
        solveResidents ([nor,eng,spani,jap,ukr]:prs)
            | nor /= first = solveResidents prs  -- Constraint 10
            | otherwise = let colors = solveColors perms
                          in if isNothing colors
                             then solveResidents prs
                             else colors
            where
                solveColors ([red,blue,green,ivory,yellow]:pcs)
                    | eng /= red ||  -- Constraint 2
                      green /= ivory + 1 ||  -- Constraint 6
                      blue /= second = solveColors pcs  -- Constraint 15 + 10
                    | otherwise = let smokes = solveSmokes perms
                                  in if isNothing smokes
                                     then solveColors pcs
                                     else smokes
                    where
                        solveSmokes ([ches,kools,lucky,parl,old]:pss)
                            | yellow /= kools ||  -- Constraint 8
                              jap /= parl = solveSmokes pss  -- Constraint 14
                            | otherwise = let drinks = solveDrinks perms
                                          in if isNothing drinks
                                             then solveSmokes pss
                                             else drinks
                            where
                                solveDrinks ([coffee, milk, oj, tea, water]:pds)
                                    | coffee /= green ||  -- Constraint 4
                                      tea /= ukr ||  -- Constraint 5
                                      milk /= middle || -- Constraint 9
                                      oj /= lucky = solveDrinks pds  -- Constraint 13
                                    | otherwise = let pets = solvePets perms
                                                  in if isNothing pets
                                                     then solveDrinks pds
                                                     else pets
                                    where
                                        solvePets ([dog,horse,fox,snails,zebra]:pps)
                                            | spani /= dog ||  -- Constraint 3
                                              snails /= old ||  -- Constraint 7
                                              not (ches `nextTo` fox) ||  -- Constraint 11
                                              not (kools `nextTo` horse) = solvePets pps  -- Constraint 12
                                            | otherwise = Just $ Solution (matchPerson water) (matchPerson zebra)
                                            where
                                                persons = [(eng, Englishman)
                                                        ,(nor, Norwegian)
                                                        ,(jap, Japanese)
                                                        ,(spani, Spaniard)
                                                        ,(ukr, Ukrainian)]
                                                matchPerson n = head [snd x | x <- persons, fst x == n]
                                        solvePets _ = Nothing
                                solveDrinks _ = Nothing
                        solveSmokes _ = Nothing
                solveColors _ = Nothing
        solveResidents _ = Nothing
