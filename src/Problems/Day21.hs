module Problems.Day21 (solution) where

import Data.Map (Map, fromList, lookup, insert)
import Text.Parsec (many1, sepEndBy1, newline, letter, try, char, string, (<|>))

import Common.Solution (Day)
import Common.Parse (AocInput, aocParse, integer)

data Operation
    = Addition
    | Subtraction
    | Multiplication
    | Division
    | Equality

data Expression
    = Immediate Integer
    | Function Operation String String
    | Function' Operation Expression Expression
    | Human

instance Show Operation where
    show Addition = "+"
    show Subtraction = "-"
    show Multiplication = "*"
    show Division = "/"
    show Equality = "="

instance Show Expression where
    show (Immediate v) = show (v)
    show (Human) = "HUMAN"
    show (Function o n m) = "(" ++ (show n) ++ " " ++ (show o) ++ " " ++ (show m) ++ ")"
    show (Function' o n m) = "(" ++ (show n) ++ " " ++ (show o) ++ " " ++ (show m) ++ ")"

apply :: Operation -> Integer -> Integer -> Integer
apply Addition = (+)
apply Subtraction = (-)
apply Multiplication = (*)
apply Division = div
apply _ = error "Invalid operation."

input :: AocInput () (Map String Expression)
input = do
    m <- sepEndBy1 monkey newline;
    return $ fromList m
    where
        name = many1 letter
        immediateExpr = do
            v <- integer;
            return $ Immediate v
        functionExpr = do
            n <- name;
            _ <- string " ";
            o <- (char '/' >> return Division) <|>
                 (char '+' >> return Addition) <|>
                 (char '*' >> return Multiplication) <|>
                 (char '-' >> return Subtraction);
            _ <- string " ";
            m <- name;
            return $ Function o n m
        monkey = do
            n <- name;
            _ <- string ": ";
            e <- (try immediateExpr) <|> (try functionExpr);
            return (n, e)

simplify :: Expression -> Expression
simplify (Function' o (Immediate u) (Immediate v)) = Immediate ((apply o) u v)
simplify (Function' Equality (Function' Division u (Immediate v)) (Immediate w)) = simplify $ Function' Equality u (Immediate (v * w))
simplify (Function' Equality (Function' Addition u (Immediate v)) (Immediate w)) = simplify $ Function' Equality u (Immediate (w - v))
simplify (Function' Equality (Function' Multiplication u (Immediate v)) (Immediate w)) = simplify $ Function' Equality u (Immediate (w `div` v))
simplify (Function' Equality (Function' Subtraction (Immediate u) v) (Immediate w)) = simplify $ Function' Equality v (Immediate (u - w))
simplify (Function' Equality (Function' Subtraction u (Immediate v)) (Immediate w)) = simplify $ Function' Equality u (Immediate (w + v))
simplify (Function' Equality (Function' Addition (Immediate u) v) (Immediate w)) = simplify $ Function' Equality v (Immediate (w - u))
simplify (Function' Equality (Function' Multiplication (Immediate u) v) (Immediate w)) = simplify $ Function' Equality v (Immediate (w `div` u))
simplify e = e

getExpr :: String -> Map String Expression -> Expression
getExpr s m
    | Just (Immediate v) <- l = Immediate v
    | Just (Function f u v) <- l = simplify $ Function' f (getExpr u m) (getExpr v m)
    | Just (Function' f u v) <- l = Function' f u v
    | Just (Human) <- l = Human
    | Nothing <- l = error "Could not find monkey!"
    where l = Data.Map.lookup s m

change2 :: Map String Expression -> Map String Expression
change2 m = insert "humn" Human m'
    where
        m' = case Data.Map.lookup "root" m of
            (Just (Function _ u v)) -> insert "root" (Function Equality u v) $ m
            _ -> error "Unexpected value for root!"

solution :: Day
solution = (
        show . getExpr "root" . aocParse input (),
        show . getExpr "root" . change2 . aocParse input ()
    )
