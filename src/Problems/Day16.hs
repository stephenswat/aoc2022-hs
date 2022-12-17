module Problems.Day16 (solution) where

import Data.Maybe (fromJust, catMaybes)
import Data.List.Extra (maximumOn)
import Data.Set (Set, fromList, toList, empty, member, insert, partition, notMember)
import Data.Map (Map, fromList, toList, lookup, fromListWith)
import Text.Parsec (sepEndBy1, sepBy1, newline, string, letter, many1, (<|>), try)
import Text.Show.Pretty (ppShow)

import Common.Solution (Day, notImplemented)
import Common.Parse (AocInput, aocParse, integer)

data Valve
    = Valve
    { flowRate :: Integer
    , tunnels :: Map String Integer
    }
    deriving Show

type ValveMap = Map String Valve

data State
    = State
    { position :: String
    , open :: Set String
    , pressure :: Integer
    , remaining :: Integer
    , valves :: ValveMap
    }
    deriving Show

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

run :: State -> State
run s
    | null nextStates = s
    | remaining s <= 0 = s
    | otherwise = maximumOn pressure . map run $ nextStates
    where
        cflowRate = flowRate . fromJust . Data.Map.lookup (position s) $ (valves s)
        openStates = if position s == "AA" && (notMember "AA" (open s)) then [s { open=insert (position s) (open s), remaining=(remaining s) - 1, pressure=(pressure s) + ((remaining s) - 1) * (flowRate . fromJust . Data.Map.lookup (position s) $ (valves s)) }] else []
        moveStates = [s { open=insert (position s) (open s), position=n, remaining=(remaining s) - (c + 1), pressure=(pressure s) + ((remaining s) - (c + 1)) * (flowRate . fromJust . Data.Map.lookup (position s) $ (valves s)) } | (n, c) <- Data.Map.toList . tunnels . fromJust . Data.Map.lookup (position s) $ (valves s), notMember n (open s)]
        nextStates = openStates ++ moveStates

reachable :: ValveMap -> Set String -> String -> [(String, Integer)]
reachable m v s =
    [(s', c) | (s', c) <- Data.Map.toList . tunnels $ q, (> 0) . flowRate . fromJust . Data.Map.lookup s' $ m, notMember s' v] ++
    [(n, c + i) | (s', c) <- Data.Map.toList . tunnels $ q, notMember s' v, (n, i) <- reachable m (insert s v) s']
    where
        q = fromJust . Data.Map.lookup s $ m

contract :: ValveMap -> ValveMap
contract m = Data.Map.fromList . catMaybes . map contract' . Data.Map.toList $ m
    where
        contract' :: (String, Valve) -> Maybe (String, Valve)
        contract' (n, v)
            | flowRate v == 0 && n /= "AA" = Nothing
            | otherwise = Just (n, v { tunnels=fromListWith min . reachable m empty $ n })

-- solve1 :: ValveMap -> Integer
solve1 v = run $ State { position="AA", open=empty, pressure=0, remaining=30, valves=v }

solution :: Day
solution = (ppShow . contract . aocParse input (), ppShow . solve1 . contract . aocParse input ())
