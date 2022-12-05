module Common.Parse (aocParse, integer, AocInput) where

import Text.Parsec (Parsec, parse, many1, digit)

type AocInput a = Parsec String () a

aocParse :: Parsec String () a -> String -> a
aocParse p s = case (parse p "" s) of
    Left err -> error (show err)
    Right v  -> v

integer :: Parsec String () Integer
integer = do {
        q <- many1 digit;
        return (read q)
    }
