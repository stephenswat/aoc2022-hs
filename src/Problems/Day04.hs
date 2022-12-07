module Problems.Day04 (solution) where

import Data.Set (Set, fromList, isSubsetOf, intersection)
import Text.Parsec (sepEndBy, newline)
import Text.Parsec.Char (char)

import Common.Solution (Day)
import Common.Parse (AocInput, aocParse, integer)

input :: AocInput () [(Set Integer, Set Integer)]
input = sepEndBy ranges newline
    where
        range = do
            l <- integer;
            _ <- char '-';
            r <- integer;
            return (fromList [l..r])
        ranges = do
            l <- range;
            _ <- char ',';
            r <- range;
            return (l, r)

solution :: Day
solution = (f p1, f p2)
    where
        f p = show . length . filter p . aocParse input ()
        p1 (a, b) = a `isSubsetOf` b || b `isSubsetOf` a
        p2 (a, b) = not . null $ intersection a b
