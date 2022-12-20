module Problems.Day19 (solution) where

import Data.Maybe (catMaybes, fromJust, isJust)
import Data.List.Extra (maximumOn)
import Data.Set (Set, fromList, toList, map, filter, unions)
import Data.Map (Map, fromList, toList, empty, insertWith, singleton, findWithDefault, unionWith, lookup)
import Text.Parsec (sepEndBy1, newline, string, sepBy1, char, try, (<|>))

import Common.Solution (Day)
import Common.Parse (AocInput, aocParse, integer)

data Resource
    = Ore
    | Clay
    | Obsidian
    | Geode
    deriving (Eq, Ord, Show)

data BluePrint
    = BluePrint
    { identifier :: Integer
    , costs :: Map Resource (Map Resource Integer)
    }
    deriving (Eq, Show)

data GameState
    = GameState
    { resources :: Map Resource Integer
    , robots :: Map Resource Integer
    }
    deriving (Eq, Ord, Show)

initialState :: GameState
initialState = GameState
    { resources=empty
    , robots=singleton Ore 1
    }

input :: AocInput () [BluePrint]
input = sepEndBy1 blueprint newline
    where
        resource = try (string "ore" >> return Ore) <|>
                   try (string "clay" >> return Clay) <|>
                   try (string "obsidian" >> return Obsidian) <|>
                   try (string "geode" >> return Geode)
        cost = do
            n <- integer;
            _ <- string " ";
            r <- resource;
            return (r, n)
        robot = do
            _ <- string "Each "
            r <- resource;
            _ <- string " robot costs ";
            c <- sepBy1 cost (string " and ");
            _ <- char '.';
            return (r, Data.Map.fromList c)
        blueprint = do
            _ <- string "Blueprint ";
            i <- integer;
            _ <- string ": ";
            r <- sepBy1 robot (char ' ');
            return BluePrint { identifier=i, costs=(Data.Map.fromList r) }

superior :: Integer -> GameState -> GameState -> Bool
superior r a b = (((findWithDefault 0 Geode (resources a)) + r * (findWithDefault 0 Geode (robots a))) >= ((findWithDefault 0 Geode (resources b)) + r * (findWithDefault 0 Geode (robots b)) + (r * (r + 1)) `div` 2)) || all (\q -> (findWithDefault 0 q . resources $ a) >= (findWithDefault 0 q . resources $ b) && (findWithDefault 0 q . robots $ a) >= (findWithDefault 0 q . robots $ b)) rs
    where
        rs = [Ore, Clay, Obsidian, Geode]

advance :: GameState -> GameState
advance s = s
    { resources=unionWith (+) (resources s) (robots s)
    }

buildRobot :: BluePrint -> Resource -> GameState -> Maybe GameState
buildRobot b r s
    | a = Just (n { robots=insertWith (+) r 1 (robots n), resources=unionWith (-) (resources n) c})
    | otherwise = Nothing
    where
        c = fromJust . Data.Map.lookup r $ (costs b)
        a = all id [m <= findWithDefault 0 r' (resources s) | (r', m) <- Data.Map.toList c]
        n = advance s

successorStates :: BluePrint -> GameState -> Set GameState
successorStates b s = Data.Set.fromList nextStates
    where
        canBuild = Prelude.filter (\r -> isJust (buildRobot b r s)) [Ore, Clay, Obsidian, Geode];
        couldBuild = Prelude.filter (\r -> isJust (buildRobot b r ((!! 20) . iterate advance $ s))) [Ore, Clay, Obsidian, Geode];
        needMore = [r | r <- [Ore, Clay, Obsidian, Geode], r == Geode || any (\c -> findWithDefault 0 r c > findWithDefault 0 r (robots s)) (costs b)];
        buildBots = catMaybes [buildRobot b r s | r <- canBuild, elem r needMore];
        nextStates = if canBuild == couldBuild then buildBots else (advance s):buildBots;

play :: BluePrint -> Integer -> Set GameState -> GameState
play _ 0 s = maximumOn (findWithDefault 0 Geode . resources) . Data.Set.toList $ s
play b r s = play b (r - 1) nss
    where
        ns = unions . Data.Set.map (successorStates b) $ s
        nss = Data.Set.filter (\x -> not . any (\y -> x /= y && superior r y x) $ ns) ns

solve1 :: [BluePrint] -> Integer
solve1 bs = sum [(identifier b) * (findWithDefault 0 Geode . resources . play b 24 $ Data.Set.fromList [initialState]) | b <- bs]

solve2 :: [BluePrint] -> Integer
solve2 bs = product [findWithDefault 0 Geode . resources . play b 32 $ Data.Set.fromList [initialState] | b <- take 3 bs]

solution :: Day
solution = (
        show . solve1 . aocParse input (),
        show . solve2 . aocParse input ()
    )
