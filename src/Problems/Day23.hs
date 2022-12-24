module Problems.Day23 (solution) where

import Control.Applicative (asum)
import Data.Maybe (isJust, fromJust)
import Data.Tuple.Extra (both)
import Data.Set (Set, fromList, toList, notMember, union, difference)

import Common.Helper (scanWhile)
import Common.Solution (Day)
import Common.Geometry (Point2D, neighbours8)
import Common.Cardinal (Direction (..), translate)

readGrid :: String -> Set Point2D
readGrid s = fromList [(x, y) | (y, r) <- zip [0..] (lines s), (x, c) <- zip [0..] r, c == '#']

getNeighbours :: Point2D -> Direction -> [Point2D]
getNeighbours p North = [translate North $ p, translate West . translate North $ p, translate East . translate North $ p]
getNeighbours p West  = [translate West $ p, translate North . translate West $ p, translate South . translate West $ p]
getNeighbours p South = [translate South $ p, translate West . translate South $ p, translate East . translate South $ p]
getNeighbours p East  = [translate East $ p, translate North . translate East $ p, translate South . translate East $ p]

getMove' :: Set Point2D -> Point2D -> Direction -> Maybe Point2D
getMove' s p d
    | all (\x -> notMember x s) (getNeighbours p d) = Just (translate d p)
    | otherwise = Nothing

getMove :: Set Point2D -> Point2D -> [Direction] -> Maybe Point2D
getMove s p d
    | all (\x -> notMember x s) . neighbours8 $ p = Nothing
    | otherwise = asum [getMove' s p d' | d' <- take 4 d]

step :: (Set Point2D, [Direction]) -> ((Set Point2D, [Direction]), Bool)
step (s, d) = ((union t (difference s f), tail d), not . null $ r)
    where
        m = [(f', fromJust t') | f' <- toList s, let t' = getMove s f' d, isJust t']
        r = [i | i@(_, t') <- m, length [j | j <- m, snd j == t'] == 1]
        (f, t) = both fromList . unzip $ r

score :: Set Point2D -> Int
score s = length [(x, y) | x <- [minX..maxX], y <- [minY..maxY], notMember (x, y) s]
    where
        minX = minimum [x | (x, _) <- toList s]
        maxX = maximum [x | (x, _) <- toList s]
        minY = minimum [y | (_, y) <- toList s]
        maxY = maximum [y | (_, y) <- toList s]

solution :: Day
solution = (
        show . score . fst . (!! 10) . iterate (fst . step) . (\x -> (x, cycle [North, South, West, East])) . readGrid,
        show . length . scanWhile step . (\x -> (x, cycle [North, South, West, East])) . readGrid
    )
