module Transpose (transpose) where

-- import Data.List (zipWith)

padded :: ([String], [String]) -> ([String], [String])
padded (left, right) = let diff = length left - length right
                       in case compare diff 0 of
                          EQ -> (left, right)
                          GT -> (left, right ++ replicate diff "")
                          LT -> (left ++ replicate (-diff) " ", right)

transpose :: [String] -> [String]
transpose [] = []
transpose (line:ls) = uncurry (zipWith (++)) $ padded (map (: "") line, transpose ls)
