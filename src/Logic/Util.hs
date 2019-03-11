module Logic.Util where

sign :: Int -> Int
sign x
    | x == 0 = 0
    | x < 0 = -1
    | otherwise = 1

