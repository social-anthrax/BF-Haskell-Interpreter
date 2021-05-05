module Parenthesis (balanced) where
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe ( isJust )

matchingParens :: Map Char Char
matchingParens = Map.fromList [
         ('(', ')')
        ,('[',']')
        ,('{', '}')
    ]

isOpening :: Char -> Bool
isOpening c = Data.Maybe.isJust $ Map.lookup c matchingParens

type Stack a = [a]

balanced :: String -> Bool
balanced = balanced' []
    where
        balanced' :: Stack Char -> String -> Bool
        balanced' [] "" = True
        balanced' _ ""  = False
        balanced' [] (c:cs) = balanced' [c] cs
        balanced' (o:os) (c:cs)
            | isOpening c = balanced' (c:o:os) cs
            | otherwise = case Map.lookup o matchingParens of
                Nothing -> False
                Just closing -> (closing == c) && balanced' os cs