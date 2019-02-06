module RunLength (decode, encode) where

import Data.Char

strToInt :: String -> Int
strToInt x = read x :: Int

encode :: String -> String
encode "" = ""
encode (x:xs) = let (left, right) = span (==x) (x:xs)
                in case length left of
    1 -> x : encode right
    _ -> show (length left) ++ (x : encode right)

decode :: String -> String
decode "" = ""
decode (x:xs)
    | isDigit x = let (left, r:rs) = span isDigit xs
                  in replicate (strToInt (x:left)) r ++ decode rs
    | otherwise = x : decode xs
