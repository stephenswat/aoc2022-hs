module Problems.Day06 (solution) where

import Data.List (nub)

import Common.Solution (Day)

segmentsOf :: Int -> [a] -> [[a]]
segmentsOf n xs
    | length xs >= n = (take n xs):(segmentsOf n (tail xs))
    | otherwise = []

solution :: Day
solution = (f 4, f 14)
    where
        f n = show . fst . head . filter (\(_, x) -> x == nub x) . zip [n..] . segmentsOf n
