module Problems.Day14 (solution) where

import Text.Parsec (sepEndBy1, sepBy1, string, newline, char)
import Data.Map (fromList, filter, size, findWithDefault, insert, toList)

import Common.Solution (Day)
import Common.Geometry (Grid2D, Point2D)
import Common.Parse (AocInput, integer, aocParse)
import Common.Helper (pairwise, runWhile)

data Tile = Empty | Rock | Sand deriving (Eq, Show)

online :: Point2D -> Point2D -> [Point2D]
online (x1, y1) (x2, y2) =
    [ (x', y')
    | x' <- [(min x1 x2)..(max x1 x2)]
    , y' <- [(min y1 y2)..(max y1 y2)]
    ]

input :: AocInput () (Grid2D Tile)
input = do
    lns <- sepEndBy1 line newline;
    return . fromList . map (\x -> (x, Rock)) . concat . map (uncurry online) .
        concat . map pairwise $ lns;
    where
        coord = do
            x <- integer;
            _ <- char ','
            y <- integer;
            return (x, y)
        line = sepBy1 coord (string " -> ")

simulateSand :: Integer -> Grid2D Tile -> Point2D -> (Point2D, Bool)
simulateSand m g p@(x, y)
    | y >= m = (p, False)
    | (findWithDefault Empty (x, y + 1) g) == Empty = ((x, y + 1), True)
    | (findWithDefault Empty (x - 1, y + 1) g) == Empty = ((x - 1, y + 1), True)
    | (findWithDefault Empty (x + 1, y + 1) g) == Empty = ((x + 1, y + 1), True)
    | otherwise = (p, False)

addSand1 :: Integer -> Grid2D Tile -> (Grid2D Tile, Bool)
addSand1 m g
    | y >= m = (g, False)
    | otherwise = (insert p Sand g, True)
    where
        p@(_, y) = runWhile (simulateSand m g) (500, 0)

addSand2 :: Integer -> Grid2D Tile -> (Grid2D Tile, Bool)
addSand2 m g = let p = runWhile (simulateSand m g) (500, 0) in (insert p Sand g, p /= (500, 0))

addWall :: Grid2D Tile -> Grid2D Tile
addWall g = foldl (\a b -> insert b Rock a) g [(i, m + 2) | i <- [-10000..10000]]
    where
        m = maximum . map (snd . fst) . toList $ g

solution :: Day
solution = (f (runWhile (addSand1 200)), f (runWhile (addSand2 200) . addWall))
    where
        f g = show . size . Data.Map.filter (== Sand) . g . aocParse input ()
