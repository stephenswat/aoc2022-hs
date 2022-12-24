module Common.Search (bfs) where

import Data.Set (Set, map, unions)

bfs :: Ord c => (c -> Bool) -> (s -> s) -> (s -> c -> Set c) -> s -> Set c -> Integer
bfs a t f s p
    | any a p = 0
    | otherwise = 1 + (bfs a t f ns (unions . Data.Set.map (f ns) $ p))
    where
        ns = t s
