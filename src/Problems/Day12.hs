module Problems.Day12 (solution) where

import Data.Either ()
import Data.Char (ord)
import Data.Maybe (isJust, fromJust)
import Data.Set (notMember, insert, singleton)
import Data.Map (lookup, toList, filter)

import Common.Solution (Day)
import Common.Geometry (Grid2D, Point2D, readGrid2DWith, neighbours4)

data Tile = Start | End | Height Int deriving (Show, Eq)

parse :: String -> Grid2D Tile
parse = readGrid2DWith f
    where
        f 'S' = Start
        f 'E' = End
        f c = Height ((ord c) - (ord 'a'))

height :: Tile -> Int
height (Height a) = a
height (Start) = (ord 'a') - (ord 'a')
height (End) = (ord 'z') - (ord 'a')

solve :: (Tile -> Bool) -> (Tile -> Tile -> Bool) -> Grid2D Tile -> Point2D -> Maybe Int
solve e m g s = f [(s, 0)] $ (singleton s)
    where
        f ((p, c):xs) v
            | (isJust q) && (e . fromJust $ q) = Just c
            | otherwise = f (xs ++ xss) (foldl (flip insert) v . map fst $ xss)
            where
                q = Data.Map.lookup p g
                xss =
                    [ (p', c + 1)
                    | p' <- neighbours4 p
                    , isJust . Data.Map.lookup p' $ g
                    , notMember p' v
                    , m (fromJust q) (fromJust . Data.Map.lookup p' $ g)
                    ]
        f [] _ = Nothing

solution :: Day
solution = (
        go (== End) (== Start) (\f t -> height t <= (height f) + 1),
        go ((== height Start) . height) (== End) (\f t -> (height t) + 1 >= (height f))
    )
    where
        go e s m i = let g = parse i in show . solve e m g . fst . head . toList . Data.Map.filter s $ g
