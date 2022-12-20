module Problems.Day17 (solution) where

import Data.List (find)
import Data.Set (Set, empty, fromList, toList, map, notMember, union, member, filter, map)
import Data.Map (Map, insert, empty, lookup)
import Text.Parsec (many, char, (<|>))

import Common.Solution (Day)
import Common.Geometry (Point2D)
import Common.Parse (AocInput, aocParse)
import Common.Helper (runWhile)

data GameState
    = GameState
    { field :: Set Point2D
    , shapeIndex :: Integer
    , windIndex :: Integer
    }
    deriving (Eq, Ord, Show)

data Move = West | East deriving (Eq, Show)

getPiece :: Integer -> Set Point2D
getPiece 0 = fromList [(2, 0), (3, 0), (4, 0), (5, 0)]
getPiece 1 = fromList [(3, 0), (2, 1), (3, 1), (4, 1), (3, 2)]
getPiece 2 = fromList [(2, 0), (3, 0), (4, 0), (4, 1), (4, 2)]
getPiece 3 = fromList [(2, 0), (2, 1), (2, 2), (2, 3)]
getPiece 4 = fromList [(2, 0), (2, 1), (3, 0), (3, 1)]
getPiece _ = error "Invalid piece index."

trim :: GameState -> (GameState, Integer)
trim s
    | Just y <- r = (s { field=Data.Set.filter ((>= 1) . snd) . Data.Set.map (\(x', y') -> (x', y' - y)) . field $ s }, y)
    | otherwise = (s, 0)
    where
        r = find (\y -> all (\x -> member (x, y) (field s)) [0..6]) . reverse $ [0..(fieldHeight s)]

input :: AocInput () [Move]
input = many ((char '<' >> return West) <|> (char '>' >> return East))

fieldHeight :: GameState -> Integer
fieldHeight = maximum . (0:) . fmap snd . toList . field

pieceIsLegal :: Set Point2D -> GameState -> Bool
pieceIsLegal p s = all (\(x, y) -> y >= 1 && x >= 0 && x <= 6 && notMember (x, y) (field s)) p

movePieceVertically :: Integer -> Set Point2D -> Set Point2D
movePieceVertically n = Data.Set.map (\(x, y) -> (x, y + n))

movePieceHorizontally :: Move -> GameState -> Set Point2D -> Set Point2D
movePieceHorizontally m s p = if pieceIsLegal r s then r else p
    where
        t (x, y) = (if m == West then x - 1 else x + 1, y)
        r = Data.Set.map t p

playMove :: [Move] -> GameState -> (Set Point2D, Integer) -> ((Set Point2D, Integer), Bool)
playMove m s (p, w)
    | not . pieceIsLegal p'' $ s = ((p', w'), False)
    | otherwise = ((p'', w'), True)
    where
        p' = movePieceHorizontally (m !! (fromIntegral w)) s p
        p'' = movePieceVertically (-1) p'
        w' = (w + 1) `mod` (fromIntegral . length $ m)

playRound :: [Move] -> GameState -> GameState
playRound m s = s
    { shapeIndex=((shapeIndex s) + 1) `mod` 5
    , windIndex=fw
    , field=union fp (field s)
    }
    where
        (fp, fw) = runWhile (playMove m s) (movePieceVertically ((+ 4) . fieldHeight $ s) . getPiece . shapeIndex $ s, windIndex s)

solve :: Integer -> [Move] -> Integer
solve n m = go n 0 Data.Map.empty $ GameState { field=Data.Set.empty, shapeIndex=0, windIndex=0 }
    where
        go :: Integer -> Integer -> Map GameState (Integer, Integer) -> GameState -> Integer
        go 0 b _ s = (fieldHeight s) + b
        go i b h s
            | Just (oi, oh) <- Data.Map.lookup s h = let r = i `div` (oi - i) in
                if r >= 1
                then go (i - r * (oi - i)) (b + r * (b - oh)) h s
                else go (i - 1) (b + tv) (insert s (i, (b + tv)) h) t
            | otherwise = go (i - 1) (b + tv) (insert s (i, (b + tv)) h) t
            where
                (t, tv) = trim (playRound m s)

solution :: Day
solution = (
        show . solve 2022 . aocParse input (),
        show . solve 1000000000000 . aocParse input ()
    )
