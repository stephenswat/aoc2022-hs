module Problems.Day02 (solution) where

import Common.Solution (Day)

data Move = Rock | Paper | Scissors deriving (Show, Eq)

data Result = Win | Draw | Loss deriving Show

parseMove :: Char -> Move
parseMove 'A' = Rock
parseMove 'B' = Paper
parseMove 'C' = Scissors
parseMove 'X' = Rock
parseMove 'Y' = Paper
parseMove 'Z' = Scissors
parseMove _ = error "Invalid move"

parseResult :: Char -> Result
parseResult 'X' = Loss
parseResult 'Y' = Draw
parseResult 'Z' = Win
parseResult _ = error "Invalid result"

parseLine1 :: String -> (Move, Move)
parseLine1 s = (parseMove (s !! 0), parseMove (s !! 2))

parseLine2 :: String -> (Move, Result)
parseLine2 s = (parseMove (s !! 0), parseResult (s !! 2))

supr :: Move -> Move
supr Rock = Paper
supr Paper = Scissors
supr Scissors = Rock

toPlay :: Move -> Result -> Move
toPlay a Draw = a
toPlay a Win = supr a
toPlay a Loss = supr . supr $ a

scoreGame :: Move -> Move -> Int
scoreGame o m = case m of {Rock -> 1; Paper -> 2; Scissors -> 3} +
    (if m == o then 3 else (if m == supr o then 6 else 0))

solution :: Day
solution = (
        show . sum . map (uncurry scoreGame) . map parseLine1 . lines,
        show . sum . map (uncurry scoreGame) . map (\(m, o) -> (m, toPlay m o)) . map (parseLine2) . lines
    )
