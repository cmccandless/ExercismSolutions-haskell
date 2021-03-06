module Triplet (isPythagorean, mkTriplet, pythagoreanTriplets) where

isPythagorean :: (Int, Int, Int) -> Bool
isPythagorean (a, b, c) = a * a + b * b == c * c

mkTriplet :: Int -> Int -> Int -> (Int, Int, Int)
mkTriplet a b c 
    | a > b = mkTriplet b a c
    | b > c = mkTriplet a c b
    | otherwise = (a, b, c)
    
genTriplits :: Int -> Int -> Int -> [(Int, Int, Int)]
genTriplits m a (-1) = concatMap (genTriplits m a) [a..m]
genTriplits m a b = filter isPythagorean $ map (mkTriplet a b) [b..m]

pythagoreanTriplets :: Int -> Int -> [(Int, Int, Int)]
pythagoreanTriplets minF maxF = concatMap f [minF..maxF]
    where
        f a = genTriplits maxF a (-1)
