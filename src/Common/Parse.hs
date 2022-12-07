module Common.Parse (aocParse, integer, AocInput) where

import Text.Parsec (Parsec, runParser, many1, digit)

type AocInput s a = Parsec String s a

aocParse :: Parsec String s a -> s -> String -> a
aocParse p s i = case (runParser p s "" i) of
    Left err -> error (show err)
    Right v  -> v

integer :: Parsec String a Integer
integer = do {
        q <- many1 digit;
        return (read q)
    }
