module Problems.Day16 (solution) where

import Data.Maybe (fromJust, catMaybes)
import Data.List.Extra (maximumOn)
import Data.List.Index (modifyAt)
import Data.Set (Set, empty, insert, notMember)
import Data.Map (Map, fromList, toList, lookup, fromListWith)
import Text.Parsec (sepEndBy1, sepBy1, newline, string, letter, many1, (<|>), try)

import Common.Solution (Day)
import Common.Parse (AocInput, aocParse, integer)

data Valve
    = Valve
    { flowRate :: Integer
    , tunnels :: Map String Integer
    }
    deriving Show

type ValveMap = Map String Valve

data Status
    = Status
    { agents :: [(String, [Move])]
    , open :: (Set String)
    , pressure :: Integer
    , remaining :: Integer
    }
    deriving Show

data Move
    = Move String
    | Open
    | Wait
    deriving (Eq, Show)

input :: AocInput () ValveMap
input = do
    v <- sepEndBy1 valve newline;
    return (Data.Map.fromList v)
    where
        valve = do
            _ <- string "Valve ";
            n <- many1 letter;
            _ <- string " has flow rate=";
            f <- integer;
            _ <- (try $ string "; tunnels lead to valves ") <|> (try $ string "; tunnel leads to valve ");
            t <- sepBy1 (many1 letter) (string ", ");
            return (n, Valve { flowRate=f, tunnels=Data.Map.fromList . map (\x -> (x, 1)) $ t })

applyMove :: ValveMap -> Int -> Move -> Status -> Status
applyMove _ _ (Wait) s = s
applyMove v a (Open) s = s
    { open=insert (fst . (!! a) . agents $ s) (open s)
    , pressure=(pressure s) + (if notMember (fst . (!! a) . agents $ s) (open s) then ((remaining s) - 1) * (flowRate . fromJust . Data.Map.lookup (fst . (!! a) . agents $ s) $ v) else 0)
    }
applyMove _ a (Move t) s = s
    { agents=modifyAt a (\(_, q) -> (t, q)) (agents s)
    }

advanceTime :: ValveMap -> Status -> Status
advanceTime v s = n { remaining=(remaining n) - 1, agents=[(p, tail q) | (p, q) <- agents n] }
    where
        mvs = [(i, head q) | (i, (_, q)) <- zip [0..] (agents s)]
        n = foldl (\s' (i, m) -> applyMove v i m s') s mvs

candidateStates :: ValveMap -> Int -> Status -> [Status]
candidateStates v n t
    | n == 0 = ts
    | otherwise = concat . map (candidateStates v (n - 1)) $ ts
    where
        a = (!! n) . agents $ t
        ts = if (null . snd $ a) then [t { agents=modifyAt n (\(p, _) -> (p, q)) (agents t) } | q <- getMoves v a t] else [t]

getMoves :: ValveMap -> (String, [Move]) -> Status -> [[Move]]
getMoves v (ap, _) s = if null moves then [[Wait]] else moves
    where
        openMoves = if (ap == "AA") && (notMember "AA" (open s)) && (not . any (\(p, q) -> p == "AA" && elem Open q) . agents $ s) then [[Open]] else []
        moveMoves =
            [ (replicate (fromIntegral c - 1) Wait) ++ [Move t] ++ [Open]
            | (t, c) <- Data.Map.toList . tunnels . fromJust . Data.Map.lookup (ap) $ v
            , notMember t (open s)
            , (not . any (\x -> (elem (Move t) . snd $ x) || ((snd x) == [Open] && (fst x) == t)) . agents $ s)
            ]
        moves = openMoves ++ moveMoves

run :: ValveMap -> Status -> Status
run v s
    | remaining s <= 0 = s
    | otherwise = maximumOn pressure . map (run v) . map (advanceTime v) $ candidateStates v ((length . agents $ s) - 1) s



contract :: ValveMap -> ValveMap
contract m = Data.Map.fromList . catMaybes . map contract' . Data.Map.toList $ m
    where
        reachable v s =
            (if s == "AA" || (flowRate q) > 0 then [(s, 0)] else []) ++
            [(n, c + i) | (s', c) <- tnls, notMember s' v, (n, i) <- reachable (insert s v) s']
            where
                q = fromJust . Data.Map.lookup s $ m
                tnls = Data.Map.toList . tunnels $ q
        contract' (n, v)
            | flowRate v == 0 && n /= "AA" = Nothing
            | otherwise = Just (n, v { tunnels=fromListWith min . reachable empty $ n })

solution :: Day
solution = (f m1, f m2)
    where
        m1 = Status { agents=[("AA", [])], open=empty, pressure=0, remaining=30 }
        m2 = Status { agents=[("AA", []), ("AA", [])], open=empty, pressure=0, remaining=26 }
        f m s = let i = contract . aocParse input () $ s in show . pressure . run i $ m
