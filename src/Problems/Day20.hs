module Problems.Day20 (solution) where

import Data.Maybe (fromJust)
import Data.List.Index (insertAt, deleteAt, ifind)
import Text.Parsec (sepEndBy1, newline)

import Common.Solution (Day)
import Common.Parse (AocInput, aocParse, integer)
import Common.Modular ((!!%))

input :: AocInput () [Int]
input = do
    r <- sepEndBy1 integer newline;
    return . map fromIntegral $ r

shift :: [(Int, Int)] -> Int -> [(Int, Int)]
shift l i = insertAt ((o + v) `mod` (length l')) (k, v) $ l'
    where
        (o, (k, v)) = fromJust . ifind (\_ (k', _) -> k' == i) $ l
        l' = deleteAt o l

solve :: Int -> Int -> [Int] -> Int
solve m n l = (f !!% (i + 1000)) + (f !!% (i + 2000)) + (f !!% (i + 3000))
    where
        l' = map (* m) l
        f = map snd . foldl shift (zip [0..] l') . concat $ [[0..(length $ l') - 1] | _ <- [1..n]]
        (i, _) = fromJust . ifind (\_ v -> v == 0) $ f

solution :: Day
solution = (
        show . solve 1 1 . aocParse input (),
        show . solve 811589153 10 . aocParse input ()
    )
