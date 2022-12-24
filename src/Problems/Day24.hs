module Problems.Day24 (solution) where

import Data.Set (Set, fromList, singleton, map, unions, member, notMember)
import Text.Parsec (sepEndBy1, many, newline, char, (<|>))

import Common.Parse (AocInput, aocParse)
import Common.Solution (Day)
import Common.Geometry (Point2D, neighbours5)
import Common.Cardinal (Direction (..), Rotation (..), rotate)

type Blizzard = (Point2D, Direction)

type Point2DL = (Point2D, Integer)

translate :: Direction -> Point2D -> Point2D
translate North (x, y) = (x, y - 1)
translate West  (x, y) = (x - 1, y)
translate South (x, y) = (x, y + 1)
translate East  (x, y) = (x + 1, y)

input :: AocInput () (Set Point2D, [Blizzard])
input = do
    rs <- sepEndBy1 (many tile) newline;
    let t = [((x, y), c) | (y, r) <- zip [0..] rs, (x, Left c) <- zip [0..] r];
    let b = [((x, y), c) | (y, r) <- zip [0..] rs, (x, Right c) <- zip [0..] r];
    return (fromList ([c | (c, v) <- t, v] ++ [c | (c, _) <- b]), b)
    where
        tile =
            (char '.' >> return (Left True)) <|>
            (char '#' >> return (Left False)) <|>
            (char '^' >> return (Right North)) <|>
            (char '<' >> return (Right West)) <|>
            (char 'v' >> return (Right South)) <|>
            (char '>' >> return (Right East))

stepBlizzard :: Set Point2D -> Blizzard -> Blizzard
stepBlizzard m (c, d)
    | member nc m = (nc, d)
    | otherwise = (nc', d)
    where
        nc = translate d c
        b = rotate RotateLeft . rotate RotateLeft $ d
        nc' = translate d . head . filter (\x -> notMember x m) . iterate (translate b) $ c

bfs' :: Set Point2D -> [Blizzard] -> Point2DL -> Set Point2DL
bfs' m b (p, l) = fromList [(c, nl c l) | c <- neighbours5 p , not . any ((== c) . fst) $ b, member c m]
    where
        nl (150, 21) 0 = 1
        nl (1, 0) 1 = 2
        nl _ l' = l'

bfs :: (Point2DL -> Bool) -> Set Point2D -> [Blizzard] -> Set Point2DL -> Integer
bfs a m b p
    | any a p = 0
    | otherwise = 1 + (bfs a m nb (unions . Data.Set.map (bfs' m nb) $ p))
    where
        nb = fmap (stepBlizzard m) b

solve :: (Point2DL -> Bool) -> Set Point2D -> [Blizzard] -> Integer
solve a m b = bfs a m b (singleton ((1, 0), 0))

solution :: Day
solution = (
        show . uncurry (solve (== ((150, 21), 1))) . aocParse input (),
        show . uncurry (solve (== ((150, 21), 2))) . aocParse input ()
    )
