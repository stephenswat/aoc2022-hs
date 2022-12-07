module Problems.Day07 (solution) where

import Data.List.Extra (groupOnKey, partition)
import Data.Map (Map, fromList, toList)
import Data.Maybe (catMaybes)
import Text.Parsec (newline, letter, many1, char, (<|>), string, setState, getState, updateState, eof)

import Common.Solution (Day)
import Common.Parse (AocInput, aocParse, integer)

data Node
    = DirNode { children :: Map String Node }
    | FileNode { size :: Integer }
    deriving Show

data File = File { path :: [String], name :: String, isize :: Integer }
    deriving Show

input :: AocInput [String] [File]
input = do
        c <- many1 command;
        _ <- eof;
        return (concat c)
    where
        command = do
            _ <- string "$ "
            c <- commandCd <|> commandLs
            return c
        commandCd = do
            _ <- string "cd ";
            s <- (many1 letter) <|> (string "/") <|> (string "..");
            _ <- newline;
            case s of
                "/"  -> setState []
                ".." -> updateState tail
                _    -> updateState ((:) s);
            return []
        lsResultDir = do
            _ <- string "dir ";
            _ <- many1 letter;
            return Nothing
        lsResultFile = do
            s <- integer;
            _ <- char ' ';
            n <- many1 (letter <|> char '.');
            p <- getState;
            return (Just (File { path=p, name=n, isize=s }))
        commandLs = do
            _ <- string "ls";
            _ <- newline;
            r <- many1 (do
                    q <- lsResultDir <|> lsResultFile;
                    _ <- newline;
                    return q
                )
            return (catMaybes r)

makeTree :: [File] -> Node
makeTree fs = DirNode { children=fromList (fls ++ drs) }
    where
        (files, dirs) = partition (null . path) fs
        fls = [(name n, FileNode { size=isize n }) | n <- files]
        drs = [(d, makeTree [x { path=init . path $ x } | x <- ns]) | (d, ns) <- groupOnKey (last . path) dirs]

getSize :: Node -> Integer
getSize (FileNode { size=x }) = x
getSize (DirNode { children=x }) = sum . map (getSize . snd) . toList $ x

findDirs :: (Node -> Bool) -> Node -> [Node]
findDirs p d@(DirNode { children=x }) = (if (p d) then [d] else []) ++ (concat . map ((findDirs p) . snd) . toList $ x)
findDirs _ _ = []

solve1 :: Node -> Integer
solve1 = sum . map getSize . findDirs ((<= 100000) . getSize)

solve2 :: Node -> Integer
solve2 d@(DirNode {}) = minimum . map getSize . findDirs ((>= toFree) . getSize) $ d
    where
        toFree = 30000000 - (70000000 - getSize d)
solve2 _ = 0

solution :: Day
solution = (f solve1, f solve2)
    where
        f g = show . g . makeTree . aocParse input []
