{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Main where

import Data.Functor

parse :: String -> [[Int]]
parse = ((read<$>) . words <$>) . lines

ascend :: [Int] -> Bool
ascend (x:y:xs) = x-y <= 3 && x-y > 0 && ascend (y:xs)
ascend _        = True
descend :: [Int] -> Bool
descend (x:y:xs) = x-y < 0 && x-y >= -3 && descend (y:xs)
descend _        = True
check :: [Int] -> Bool
check (x:y:xs)        |  ascend (checkWithMissing (x:y:xs)) || descend (checkWithMissing (x:y:xs)) = True
                      |  otherwise = False
checkWithMissing ::  [Int] -> [Int]
checkWithMissing xs =
    case filter (\(_, ys) -> ascend ys || descend ys) (zip xs (removeEach xs)) of
        ((x, ys):_) -> ys
        []          -> xs

removeEach :: [Int] -> [[Int]]
removeEach xs = [take i xs ++ drop (i+1) xs | i <- [0..length xs - 1]]
solve :: [[Int]] -> Int
solve [] = 0
solve (x:xs)    | check x =  solve xs + 1
                | otherwise = solve xs


main :: IO()
main = readFile "inputs" >>= (print . solve) . parse

