module Problems.Day13 (solution) where

import Data.List (sort)
import Data.Tuple.Extra ((&&&))
import Text.Parsec (sepEndBy1, sepBy, between, char, newline, (<|>))

import Common.Solution (Day)
import Common.Parse (AocInput, aocParse, integer)

data Item = List [Item] | Value Integer deriving (Show, Eq)

instance Ord Item where
    compare (List l) (List r) = compare l r
    compare (Value l) (Value r) = compare l r
    compare l@(Value _) r@(List _) = compare (List [l]) r
    compare l@(List _) r@(Value _) = compare l (List [r])

input :: AocInput () [(Item, Item)]
input =  sepEndBy1 pair newline
    where
        itemVal = integer >>= (return . Value)
        itemList = do
            v <- between (char '[') (char ']') (sepBy (itemList <|> itemVal) (char ','))
            return (List v)
        pair = do
            l <- (itemVal <|> itemList);
            _ <- newline;
            r <- (itemVal <|> itemList);
            _ <- newline;
            return (l, r)

solution :: Day
solution = (
        show . sum . map fst . filter (uncurry (<=) . snd) . zip [(1 :: Integer)..] .
            aocParse input (),
        show . product . map fst .
            filter ((uncurry (||)) . ((== d1) &&& (== d2)) . snd) . zip [(1 :: Integer)..] .
            sort . (++ [d1, d2]) . concat . map (\(a, b) -> [a, b]) .
            aocParse input ()
    )
    where
        d1 = List [List [Value 2]]
        d2 = List [List [Value 6]]
