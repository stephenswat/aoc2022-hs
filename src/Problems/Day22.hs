module Problems.Day22 (solution) where

import Data.List (uncons)
import Data.Map (fromList, toList, findWithDefault)
import Text.Parsec ((<|>), sepEndBy1, many, newline, char)

import Common.Solution (Day, notImplemented)
import Common.Geometry (Grid2D, Point2D)
import Common.Parse (AocInput, aocParse, integer)
import Common.Cardinal (Direction (..), Rotation (..), rotate)

data Tile = Open | Wall | Void deriving (Eq, Show)

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

input :: AocInput () (Grid2D Tile, [Move])
input = do
    rs <- sepEndBy1 row newline;
    ms <- many move;
    return (fromList [((x, y), t) | (y, r) <- zip [0..] rs, (x, t) <- zip [0..] r], ms)
    where
        tile = (char '.' >> return Open) <|>
               (char '#' >> return Wall) <|>
               (char ' ' >> return Void)
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

translate :: Direction -> Point2D -> Point2D
translate North (x, y) = (x, y - 1)
translate West  (x, y) = (x - 1, y)
translate South (x, y) = (x, y + 1)
translate East  (x, y) = (x + 1, y)

moveForward :: Grid2D Tile -> Direction -> Point2D -> Point2D
moveForward g d p
    | t == Open = m
    | t == Wall = p
    | otherwise = error "Invalid!"
    where
        n = translate d p
        b = rotate RotateLeft . rotate RotateLeft $ d
        m = if findWithDefault Void n g == Void
            then translate d . head . filter (\x -> findWithDefault Void x g == Void) . iterate (translate b) $ p
            else n
        t = findWithDefault Void m g

playMove :: Grid2D Tile -> GameState -> GameState
playMove g s
    | Nothing <- r = s
    | Just (Turn d, t) <- r = s { facing=rotate d (facing s), moves=t }
    | Just (Forward 0, t) <- r = s { moves=t }
    | Just (Forward n, t) <- r = s { position=moveForward g (facing s) (position s), moves=if n <= 1 then t else Forward (n - 1):t }
    where
        r = uncons . moves $ s

scoreFacing :: Direction -> Integer
scoreFacing North = 3
scoreFacing West  = 2
scoreFacing South = 1
scoreFacing East  = 0

scoreState :: GameState -> Integer
scoreState s = 1000 * ((+1) . snd . position $ s) + 4 * ((+1) . fst . position $ s) + scoreFacing (facing s)

play :: Grid2D Tile -> [Move] -> GameState
play g m = until (null . moves) (playMove g) . initGame g $ m

solution :: Day
solution = (show . scoreState . uncurry play . aocParse input (), notImplemented)
