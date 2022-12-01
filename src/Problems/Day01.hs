module Problems.Day01 (solution) where

import Data.List (sort)
import Data.List.Split (splitWhen)
import Common.Solution (Day)

groups :: String -> [[Int]]
groups = map (map read) . splitWhen null . lines

solution :: Day
solution = (
        show . maximum . map sum . groups,
        show . sum . take 3 . reverse . sort . map sum . groups
    )
