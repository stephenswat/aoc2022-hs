module Problems.Day18 (solution) where

import Control.Monad (forM)
import Control.Monad.State.Lazy (State, evalState, modify, get)
import Data.Set (Set, empty, insert, fromList, toList, member, notMember)
import Data.Map (Map, empty, insert, lookup)
import Text.Parsec (sepEndBy1, newline, char)

import Common.Solution (Day)
import Common.Parse (AocInput, aocParse, integer)
import Common.Geometry (Point3D, neighbours6)

input :: AocInput () (Set Point3D)
input = do
    pts <- sepEndBy1 point newline;
    return . fromList $ pts
    where
        point = do
            x <- integer;
            _ <- char ',';
            y <- integer;
            _ <- char ',';
            z <- integer;
            return (x, y, z)

solve1 :: Set Point3D -> Int
solve1 s = length . filter (\x -> notMember x s) . concat . map neighbours6 . toList $ s

solve2 :: Set Point3D -> Int
solve2 s = length . filter (\x -> evalState (reachesOutside x) (Data.Set.empty, Data.Map.empty)) . concat . map neighbours6 . toList $ s
    where
        reachesOutside :: Point3D -> State (Set Point3D, Map Point3D Bool) Bool
        reachesOutside p@(x, y, z)
            | x < 0 || y < 0 || z < 0 || x >= 20 || y >= 20 || z >= 20 = return True
            | member p s = return False
            | otherwise = do
                (v, m) <- get;
                if member p v
                then return False
                else case Data.Map.lookup p m of
                    (Just b) -> return b;
                    (Nothing) -> do
                        modify (\(v', m') -> (Data.Set.insert p v', m'));
                        n <- forM (neighbours6 p) reachesOutside;
                        let r = any id n;
                        modify (\(v', m') -> (v', Data.Map.insert p r m'));
                        return r

solution :: Day
solution = (show . solve1 . aocParse input (), show . solve2 . aocParse input ())
