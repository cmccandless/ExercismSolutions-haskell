module Queens (boardString, canAttack) where

boardSquare :: Maybe (Int, Int) -> Maybe (Int, Int) -> Int -> Int -> String
boardSquare w b y x 
    | (== Just (y, x)) w = "W"
    | (== Just (y, x)) b = "B"
    | otherwise           = "_"
    
boardRow :: Maybe (Int, Int) -> Maybe (Int, Int) -> Int -> String
boardRow w b y = unwords $ map (boardSquare w b y) [0..7]

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = unlines $ map (boardRow white black) [0..7]

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (wx, wy) (bx, by) = wx == bx || wy == by || abs (wx - bx) == abs (wy - by)
