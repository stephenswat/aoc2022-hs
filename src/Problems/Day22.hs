module Problems.Day22 (solution) where

import Data.Tuple.Extra (both)
import Data.List (uncons)
import Data.Map (fromList, toList, findWithDefault)
import Text.Parsec ((<|>), sepEndBy1, many, newline, char)

import Common.Solution (Day)
import Common.Geometry (Grid2D, Point2D)
import Common.Parse (AocInput, aocParse, integer)
import Common.Cardinal (Direction (..), Rotation (..), rotate, translate)

data Tile = Open | Wall deriving (Eq, Show)

data Move = Forward Integer | Turn Rotation deriving (Eq, Show)

type Transition = (Integer, Integer) -> Direction -> ((Integer, Integer), Direction)

data GameState
    = GameState
    { position :: Point2D
    , facing :: Direction
    , moves :: [Move]
    }
    deriving (Show)

side :: Point2D -> (Integer, Integer)
side (x, y) = (x `div` 50, y `div` 50)

transition1 :: Transition
transition1 (1, 0) North = ((1, 2), North)
transition1 (1, 0) West  = ((2, 0), West)
transition1 (1, 0) South = ((1, 1), South)
transition1 (1, 0) East  = ((2, 0), East)
transition1 (2, 0) North = ((2, 0), North)
transition1 (2, 0) West  = ((1, 0), West)
transition1 (2, 0) South = ((2, 0), South)
transition1 (2, 0) East  = ((1, 0), East)
transition1 (1, 1) North = ((1, 0), North)
transition1 (1, 1) West  = ((1, 1), West)
transition1 (1, 1) South = ((1, 2), South)
transition1 (1, 1) East  = ((1, 1), East)
transition1 (0, 2) North = ((0, 3), North)
transition1 (0, 2) West  = ((1, 2), West)
transition1 (0, 2) South = ((0, 3), South)
transition1 (0, 2) East  = ((1, 2), East)
transition1 (1, 2) North = ((1, 1), North)
transition1 (1, 2) West  = ((0, 2), West)
transition1 (1, 2) South = ((1, 0), South)
transition1 (1, 2) East  = ((0, 2), East)
transition1 (0, 3) North = ((0, 2), North)
transition1 (0, 3) West  = ((0, 3), West)
transition1 (0, 3) South = ((0, 2), South)
transition1 (0, 3) East  = ((0, 3), East)
transition1 _      _     = error "Invalid transition!"

transition2 :: Transition
transition2 (1, 0) North = ((0, 3), East)
transition2 (1, 0) West  = ((0, 2), East)
transition2 (1, 0) South = ((1, 1), South)
transition2 (1, 0) East  = ((2, 0), East)
transition2 (2, 0) North = ((0, 3), North)
transition2 (2, 0) West  = ((1, 0), West)
transition2 (2, 0) South = ((1, 1), West)
transition2 (2, 0) East  = ((1, 2), West)
transition2 (1, 1) North = ((1, 0), North)
transition2 (1, 1) West  = ((0, 2), South)
transition2 (1, 1) South = ((1, 2), South)
transition2 (1, 1) East  = ((2, 0), North)
transition2 (0, 2) North = ((1, 1), East)
transition2 (0, 2) West  = ((1, 0), East)
transition2 (0, 2) South = ((0, 3), South)
transition2 (0, 2) East  = ((1, 2), East)
transition2 (1, 2) North = ((1, 1), North)
transition2 (1, 2) West  = ((0, 2), West)
transition2 (1, 2) South = ((0, 3), West)
transition2 (1, 2) East  = ((2, 0), West)
transition2 (0, 3) North = ((0, 2), North)
transition2 (0, 3) West  = ((1, 0), South)
transition2 (0, 3) South = ((2, 0), South)
transition2 (0, 3) East  = ((1, 2), North)
transition2 _      _     = error "Invalid transition!"

input :: AocInput () (Grid2D Tile, [Move])
input = do
    rs <- sepEndBy1 row newline;
    ms <- many move;
    return (fromList [((x, y), t) | (y, r) <- zip [0..] rs, (x, Just t) <- zip [0..] r], ms)
    where
        tile = (char '.' >> return (Just Open)) <|>
               (char '#' >> return (Just Wall)) <|>
               (char ' ' >> return (Nothing))
        move = (char 'L' >> return (Turn RotateLeft)) <|>
               (char 'R' >> return (Turn RotateRight)) <|>
               (integer >>= return . Forward)
        row = many tile

initGame :: Grid2D Tile -> [Move] -> GameState
initGame g m = GameState
    { position=(minX, minY)
    , facing=East
    , moves=m
    }
    where
        minY = minimum [y | ((_, y), t) <- toList g, t == Open]
        minX = minimum [x | ((x, y), t) <- toList g, t == Open, y == minY]

rotateGrid :: Direction -> Direction -> Point2D -> Point2D
rotateGrid d1 d2 (x, y)
    | d1 == d2 = (x, y)
    | otherwise = rotateGrid (rotate RotateLeft d1) d2 (y, -x + 49)

transition :: Transition -> (Integer, Integer) -> Direction -> Point2D -> (Point2D, Direction)
transition t s d p = ((50 * ntx + ndx, 50 * nty + ndy), nd)
    where
        ((ntx, nty), nd) = t s d
        p' = both (`mod` 50) p
        (ndx, ndy) = rotateGrid d nd p'

moveForward :: Transition -> Grid2D Tile -> Direction -> Point2D -> (Point2D, Direction)
moveForward f g d p
    | t == Open = (np, nd)
    | t == Wall = (p, d)
    | otherwise = error "Invalid!"
    where
        n = translate d p
        tf = side p
        tt = side n
        (np, nd) = if tf /= tt
            then transition f tf d n
            else (n, d)
        t = findWithDefault Wall np g

playMove :: Transition -> Grid2D Tile -> GameState -> GameState
playMove f g s
    | Nothing <- r = s
    | Just (Turn d, t) <- r = s { facing=rotate d (facing s), moves=t }
    | Just (Forward 0, t) <- r = s { moves=t }
    | Just (Forward n, t) <- r = s { position=np, facing=nd, moves=if n <= 1 then t else Forward (n - 1):t }
    where
        r = uncons . moves $ s
        (np, nd) = moveForward f g (facing s) (position s)

scoreState :: GameState -> Integer
scoreState s = 1000 * ((+1) . snd . position $ s) + 4 * ((+1) . fst . position $ s) + scoreFacing (facing s)
    where
        scoreFacing North = 3
        scoreFacing West  = 2
        scoreFacing South = 1
        scoreFacing East  = 0

play :: Transition -> Grid2D Tile -> [Move] -> GameState
play f g m = until (null . moves) (playMove f g) . initGame g $ m

solution :: Day
solution = (
        show . scoreState . uncurry (play transition1) . aocParse input (),
        show . scoreState . uncurry (play transition2) . aocParse input ()
    )
