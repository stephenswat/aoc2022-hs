module Problems.Day15 (solution) where

import Data.Set (Set, fromList, toList, union, filter)
import Text.Parsec (string, newline, sepEndBy1)

import Common.Geometry (Point2D, manhattan)
import Common.Parse (AocInput, aocParse, integer)
import Common.Solution (Day)

data Sensor
    = Sensor
    { position :: Point2D
    , exclusion :: Integer
    }
    deriving Show

excludedBy :: Point2D -> Sensor -> Bool
excludedBy p s = (manhattan p (position s)) <= (exclusion s)

edges :: Point2D -> Point2D -> Sensor -> Set Point2D
edges (xl, yl) (xh, yh) (Sensor { position=(x, y), exclusion=m }) =
    fromList . concat $ [f i j | i <- [True, False], j <- [True, False]]
    where
        f a b =
            [ (x', y')
            | i <- [0..(m + 1)]
            , let x' = x + (if a then 1 else (-1) * (m + 1 - i))
            , let y' = y + (if b then 1 else (-1)) * i
            , x' >= xl && x' <= xh && y' >= yl && y' <= yh
            ]

input :: AocInput () [Sensor]
input = sepEndBy1 sensor newline
    where
        sensor = do
            _ <- string "Sensor at x=";
            x <- integer;
            _ <- string ", y=";
            y <- integer;
            _ <- string ": closest beacon is at x="
            m <- integer;
            _ <- string ", y="
            n <- integer;
            let p = (x, y)
            return (Sensor { position=p, exclusion=manhattan p (m, n) })

solve1 :: [Sensor] -> Int
solve1 s = length [p | x <- [lo..hi], let p = (x, 2000000), any (excludedBy p) s]
    where
        lo = minimum [ (fst . position $ i) - (exclusion i) | i <- s ]
        hi = maximum [ (fst . position $ i) + (exclusion i) | i <- s ]

solve2 :: [Sensor] -> Integer
solve2 s
    | (x, y):[] <- r = x * 4000000 + y
    | otherwise = error "Point found is not singular."
    where
        f = Data.Set.filter (\x -> not . any (excludedBy x) $ s) . edges (0, 0) (4000000, 4000000)
        r = toList . foldl1 union . map f $ s

solution :: Day
solution = (show . solve1 . aocParse input (), show . solve2 . aocParse input ())
