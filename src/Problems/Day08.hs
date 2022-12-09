module Problems.Day08 (solution) where

import Data.Map (size, filterWithKey, findWithDefault, mapWithKey, toList)

import Common.Solution (Day)
import Common.Geometry (Grid2D, readGrid2D)

solve1 :: Grid2D Integer -> Int
solve1 g = size . filterWithKey f $ g
    where
        f (x, y) v = or [
                and [(findWithDefault (-1) (x, y') g) < v | y' <- [0..(y - 1)]],
                and [(findWithDefault (-1) (x, y') g) < v | y' <- [(y + 1)..100]],
                and [(findWithDefault (-1) (x', y) g) < v | x' <- [0..(x - 1)]],
                and [(findWithDefault (-1) (x', y) g) < v | x' <- [(x + 1)..100]]
            ]

countTrue :: [Bool] -> Int
countTrue [] = 0
countTrue (False:_) = 0
countTrue (True:xs) = 1 + (countTrue xs)

solve2 :: Grid2D Integer -> Int
solve2 g = maximum . map snd . toList . mapWithKey f $ g
    where
        f (x, y) v = (
                (1 + countTrue [(findWithDefault 10 (x, y') g) < v | y' <- reverse [1..(y - 1)]]) *
                (1 + countTrue [(findWithDefault 10 (x, y') g) < v | y' <- [(y + 1)..97]]) *
                (1 + countTrue [(findWithDefault 10 (x', y) g) < v | x' <- reverse [1..(x - 1)]]) *
                (1 + countTrue [(findWithDefault 10 (x', y) g) < v | x' <- [(x + 1)..97]])
            )

solution :: Day
solution = (show . solve1 . readGrid2D, show . solve2 . readGrid2D)
