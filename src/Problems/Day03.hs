module Problems.Day03 (solution) where

import Data.Char (ord)
import Data.Set (toList, intersection, fromList)
import Data.Tuple.Extra (both)
import Data.List.Split (chunksOf)

import Common.Solution (Day)

common :: String -> [Char]
common s = toList . uncurry intersection . both fromList . splitAt ((length s) `div` 2) $ s

value :: Char -> Int
value c
    | o >= ord 'a' && o <= ord 'z' = o - (ord 'a') + 1
    | o >= ord 'A' && o <= ord 'Z' = o - (ord 'A') + 27
    | otherwise = error "Invalid character!"
    where o = ord c

solution :: Day
solution = (
        f (map common),
        f (map (toList . foldl1 intersection . map fromList) . chunksOf 3)
    )
    where f g = show . sum . map value . concat . g . lines
