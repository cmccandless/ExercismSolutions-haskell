module ArmstrongNumbers (armstrong) where

digits :: Integral a => a -> [a]
digits 0 = []
digits x = x `mod` 10 : digits (x `div` 10)

armstrong :: Integral a => a -> Bool
armstrong x = let numDigits = length $ digits x
              in (==) x . sum . map (^ numDigits) $ digits x
