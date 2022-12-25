module Problems.Day25 (solution) where

import Data.List.Extra (minimumOn)
import Text.Parsec ((<|>), sepEndBy1, newline, many1, char)

import Common.Solution (Day)
import Common.Parse (AocInput, aocParse)

newtype SNAFU = SNAFU [Integer]

showDigit :: Integer -> Char
showDigit 2 = '2'
showDigit 1 = '1'
showDigit 0 = '0'
showDigit (-1) = '-'
showDigit (-2) = '='
showDigit _ = error "Invalid digit!"

instance Show SNAFU where
    show (SNAFU ([])) = "0"
    show (SNAFU (d:[])) = (showDigit d):[]
    show (SNAFU (d:dx)) = (showDigit d):(show (SNAFU dx))

input :: AocInput () [SNAFU]
input = sepEndBy1 snafu newline
    where
        snafu = do
            d <- many1 digit;
            return (SNAFU d)
        digit = (char '2' >> return 2) <|>
                (char '1' >> return 1) <|>
                (char '0' >> return 0) <|>
                (char '-' >> return (-1)) <|>
                (char '=' >> return (-2))

toDecimal :: SNAFU -> Integer
toDecimal (SNAFU x) = sum . map (\(n, v) -> v * 5 ^ n) . zip [(0 :: Integer)..] . reverse $ x

fromDecimal :: Integer -> SNAFU
fromDecimal n = go False 32 n
    where
        go f n m
            | n == 0 = SNAFU [d]
            | f || (d /= 0) = SNAFU (d:ds)
            | otherwise = SNAFU (ds)
            where
                (d, r) = minimumOn (abs . snd) [(d', m - d' * 5 ^ n) | d' <- [-2, -1, 0, 1, 2]]
                (SNAFU ds) = go (f || (d /= 0)) (n - 1) r

solution :: Day
solution = (
        show . fromDecimal . sum . map toDecimal . aocParse input (),
        const "Merry Christmas!"
    )
