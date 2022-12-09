module Problems.Day09 (solution) where

import Data.Set (Set, singleton, size, insert)
import Data.Ord (clamp)
import Text.Parsec (sepEndBy1, newline, char, (<|>))

import Common.Solution (Day)
import Common.Parse (AocInput, aocParse, integer)
import Common.Geometry (Point2D, neighbours9)

data Direction = North | West | South | East deriving (Eq, Show)

data Move = Move {
    dir :: Direction,
    cnt :: Int
} deriving Show

type Knot = (Point2D, Set Point2D)

movePoint :: Direction -> Point2D -> Point2D
movePoint North (x, y) = (x, y + 1)
movePoint West (x, y) = (x - 1, y)
movePoint South (x, y) = (x, y - 1)
movePoint East (x, y) = (x + 1, y)

followPoint :: Point2D -> Point2D -> Point2D
followPoint d@(dx, dy) s@(sx, sy)
    | elem s (neighbours9 d) = s
    | otherwise = (sx + clamp (-1, 1) (dx - sx), sy + clamp (-1, 1) (dy - sy))

input :: AocInput () [Move]
input = sepEndBy1 move newline
    where
        move = do
            d <- (char 'U' >> return North) <|>
                 (char 'D' >> return South) <|>
                 (char 'L' >> return West) <|>
                 (char 'R' >> return East)
            _ <- char ' '
            c <- integer
            return Move { dir=d, cnt=fromInteger c }

step :: Move -> [Knot] -> [Knot]
step (Move { dir=d, cnt=c }) = (!! c) . iterate substep
    where
        f (t, _) (ci, vi) = let n = followPoint t ci in (n, insert n vi)
        substep [] = error "No knots!"
        substep ((kc, kv):ks) = scanl1 f ((let n = movePoint d kc in (n, insert n kv)):ks)

solve :: Int -> [Move] -> Int
solve n = size . snd . last . foldl (flip step) (replicate n ((0, 0), singleton (0, 0)))

solution :: Day
solution = (f 2, f 10)
    where
        f n = show . solve n . aocParse input ()
