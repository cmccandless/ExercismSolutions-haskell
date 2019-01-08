module Proverb(recite) where

recite :: [String] -> String
recite [] = ""
recite (w:ws) = reciteLines (w:ws)
    where
        reciteLines (x1:x2:xs) =
            "For want of a " ++ x1 ++ " the " ++ x2 ++ " was lost.\n" ++
            reciteLines (x2:xs)
        reciteLines _ = "And all for the want of a " ++ w ++ "."
