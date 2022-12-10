module Problems.Day10 (solution) where

import Data.Set (Set, empty, insert, member)
import Data.List (intercalate)
import Control.Monad (when, unless)
import Control.Monad.State.Lazy (execState, get, modify)
import Text.Parsec (newline, sepEndBy1, (<|>), string, space)

import Common.Solution (Day)
import Common.Parse (AocInput, aocParse, integer)

data Machine = Machine
    { clock :: Int
    , pc :: Int
    , remaining :: Int
    , register :: Int
    , crt :: Set Int
    } deriving Show

data Instruction
    = Add Int
    | Nop
    deriving Show

input :: AocInput () [Instruction]
input = sepEndBy1 instruction newline
    where
        inAdd = do
            _ <- string "addx";
            _ <- space;
            v <- integer;
            return (Add (fromInteger v))
        inNop = do
            _ <- string "noop";
            return Nop
        instruction = inAdd <|> inNop

runProgram :: Int -> [Instruction] -> Machine
runProgram n i = execState step (Machine { clock=1, pc=0, remaining=(duration . head $ i), register=1, crt=empty })
    where
        duration (Add _) = 2
        duration (Nop) = 1

        step = do
            s1 <- get;

            when ((remaining s1) == 0) (do
                case (i !! (pc s1)) of
                    Add v -> modify (\t -> t { register=(register t) + v })
                    Nop -> return ()
                modify (\t@(Machine { pc=p }) -> t { pc=p + 1, remaining=duration . (!! (p + 1)) $ i }));

            s2 <- get;
            when (let p = (clock s2 - 1) `mod` 40; s = register $ s2 in (abs (p - s)) <= 1) (do
                modify (\t -> t { crt=insert ((clock s2) - 1) (crt t) }));

            unless ((clock s2) == n) (do
                modify (\t@(Machine { clock=c, remaining=r }) -> t { clock=c+1, remaining=r-1 });
                step);

solve1 :: [Int] -> [Instruction] -> Int
solve1 c i = sum [cx * (register . runProgram cx $ i) | cx <- c]

solve2 :: Machine -> String
solve2 m = intercalate "\n" [[if member (j + 40 * i) (crt m) then '#' else '.' | j <- [0..39]] | i <- [0..5]]

solution :: Day
solution = (
        show . solve1 [20, 60, 100, 140, 180, 220] . aocParse input (),
        ("\n" ++) . solve2 . runProgram 240 . aocParse input ()
    )
