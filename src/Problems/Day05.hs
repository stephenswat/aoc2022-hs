module Problems.Day05 (solution) where

import Data.List (transpose)
import Data.Maybe (catMaybes)
import Text.Parsec (sepEndBy1, sepBy1, newline, letter, (<|>), between, count)
import Text.Parsec.Char (char, string)

import Common.Solution (Day)
import Common.Parse (AocInput, aocParse, integer)

data Move = Move {
    num :: Int,
    src :: Int,
    dst :: Int
} deriving Show

input :: AocInput ([[Char]], [Move])
input = do
    crates <- count 8 row;
    _ <- string " 1   2   3   4   5   6   7   8   9 \n\n"
    moves <- sepEndBy1 move newline;
    return (map catMaybes . transpose $ crates, moves)
    where
        row = do
            c <- sepBy1 (justCrate <|> noCrate) (char ' ');
            _ <- char '\n'
            return c
        justCrate = do
            c <- between (char '[') (char ']') letter
            return (Just c)
        noCrate = do
            _ <- string "   ";
            return Nothing
        move = do
            _ <- string "move ";
            c <- integer;
            _ <- string " from ";
            s <- integer;
            _ <- string " to ";
            d <- integer;
            return Move {
                num=fromInteger c,
                src=fromInteger (s - 1),
                dst=fromInteger (d - 1)
            }

applyMove2 :: Move -> [[Char]] -> [[Char]]
applyMove2 m i
    | (src m) == (dst m) = i
    | otherwise =
        [ if j == (dst m) then q ++ l else (if j == (src m) then drop (num m) l else l)
        | (j, l) <- zip [0..] i
        ]
    where q = fst . splitAt (num m) $ (i !! (src m))

applyMove1 :: Move -> [[Char]] -> [[Char]]
applyMove1 m i = (iterate (applyMove2 (m { num=1 })) i) !! (num $ m)

solution :: Day
solution = (show . solve1 . aocParse input, show . solve2 . aocParse input)
    where
        solve1 (s, m) = map head . foldl (flip applyMove1) s $ m
        solve2 (s, m) = map head . foldl (flip applyMove2) s $ m
