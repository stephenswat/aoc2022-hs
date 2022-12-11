module Problems.Day11 (solution) where

import Data.List (sort)
import Text.Parsec (sepEndBy1, sepBy, newline, string, char, space, (<|>))

import Common.Solution (Day)
import Common.Parse (AocInput, aocParse, integer)

data Monkey = Monkey
    { items :: [Integer]
    , updateWorry :: Integer -> Integer
    , throwDivider :: Integer
    , throwTargetTrue :: Integer
    , throwTargetFalse :: Integer
    , inspected :: Integer
    }

type GameState = [Monkey]

input :: AocInput () GameState
input = do
    ms <- sepEndBy1 monkey newline;
    return ms
    where
        operandOld = do
            _ <- string "old";
            return (\x -> x)
        operandImm = do
            v <- integer;
            return (\_ -> v)
        mulOp = (char '*') >> return (\x y -> x * y)
        addOp = (char '+') >> return (\x y -> x + y)
        monkey = do
            _ <- string "Monkey ";
            _ <- integer;
            _ <- string ":\n  Starting items: ";
            i <- sepBy integer (string ", ");
            _ <- newline
            _ <- string "  Operation: new = ";
            l <- operandOld <|> operandImm;
            _ <- space;
            o <- mulOp <|> addOp;
            _ <- space;
            r <- operandOld <|> operandImm;
            _ <- string "\n  Test: divisible by "
            d <- integer;
            _ <- string "\n    If true: throw to monkey "
            a <- integer;
            _ <- string "\n    If false: throw to monkey "
            b <- integer;
            _ <- string "\n"
            return Monkey {
                items=i,
                updateWorry=(\x -> o (l x) (r x)),
                throwTargetTrue=a,
                throwTargetFalse=b,
                throwDivider=d,
                inspected=0
            }

playSubSubRound :: Integer -> Integer -> GameState -> GameState
playSubSubRound d m s
    | null . items $ om = s
    | otherwise = [
        if i == m then
            om { items=tail . items $ om, inspected=(+ 1) . inspected $ om }
        else if i == n then
            j { items=(items j) ++ [it `mod` df] }
        else
            j
        | (i, j) <- zip [0..] s]
    where
        df = product . map throwDivider $ s
        om = s !! (fromInteger m)
        it = (`div` d) . (updateWorry om) . head . items $ om
        n = if (it `mod` (throwDivider om) == 0)
            then (throwTargetTrue om)
            else (throwTargetFalse om)

playRound :: Integer -> GameState -> GameState
playRound d s = go ((toInteger . length $ s) - 1) s
    where
        playSubRound m t = (!! (length . items . (!! (fromInteger m)) $ t)) .
                           iterate (playSubSubRound d m) $ t
        go 0 = playSubRound 0
        go n = playSubRound n . go (n - 1)

solution :: Day
solution = (solve 20 3, solve 10000 1)
    where
        solve n d = show . product . take 2 . reverse . sort . map inspected .
                    (!! n) . iterate (playRound d) . aocParse input ()
