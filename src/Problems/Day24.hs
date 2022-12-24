module Problems.Day24 (solution) where

import Data.Set (Set, fromList, singleton, member, notMember)
import Text.Parsec (sepEndBy1, many, newline, char, (<|>))

import Common.Parse (AocInput, aocParse)
import Common.Solution (Day)
import Common.Geometry (Point2D, neighbours5)
import Common.Cardinal (Direction (..), Rotation (..), rotate, translate)
import Common.Search (bfs)

type Blizzard = (Point2D, Direction)

type Point2DL = (Point2D, Integer)

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

succStates :: (Set Point2D, Set Point2D, [Blizzard]) -> Point2DL -> Set Point2DL
succStates (m, b, _) (p, l) = fromList [(c, nl c l) | c <- neighbours5 p, notMember c b, member c m]
    where
        nl (150, 21) l' | l' `mod` 2 == 0 = l' + 1
        nl (  1,  0) l' | l' `mod` 2 == 1 = l' + 1
        nl         _ l' = l'

solve :: (Point2DL -> Bool) -> Set Point2D -> [Blizzard] -> Integer
solve a m b = bfs a t succStates i (singleton ((1, 0), 0))
    where
        t (m', _, b') = let nb = fmap (stepBlizzard m') b' in (m', fromList . map fst $ nb, nb)
        i = (m, fromList . map fst $ b, b)

solution :: Day
solution = (
        show . uncurry (solve (== ((150, 21), 1))) . aocParse input (),
        show . uncurry (solve (== ((150, 21), 3))) . aocParse input ()
    )
