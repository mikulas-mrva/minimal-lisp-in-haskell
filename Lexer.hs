module Lexer (Token(..), tokenize, lookAhead, accept) where
import Data.Char


data Token = TokLParen
           | TokRParen
           | TokQuote
           | TokAtom String
           | TokNum Double
           | TokEnd
    deriving (Show, Eq)

lookAhead :: [Token] -> Token
lookAhead [] = TokEnd
lookAhead (t:ts) = t

isAtomChar :: Char -> Bool
isAtomChar c = (isAlpha c) || (isDigit c) || ( not . null $ filter (\x -> c == x) "-_!+*/$%&|:<>=?@^#~\"")

isDDigit :: Char -> Bool
isDDigit c = (isDigit c) || (c == '.') || (c == '-')

accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (t:ts) = ts

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs) 
    | c == '('     = TokLParen : tokenize cs
    | c == ')'     = TokRParen : tokenize cs
    | c == '\''    = TokQuote  : tokenize cs
    | isDDigit c   = number c cs
    | isAtomChar c = identifier c cs
    | isSpace c    = tokenize cs
    | otherwise    = error $ "Cannot tokenize " ++ [c]
    
identifier :: Char -> String -> [Token]
identifier c cs = let (name, cs') = span isAtomChar cs in
                  TokAtom (c:name) : tokenize cs'

                  
count :: Eq a => a -> [a] -> Int
count needle [] = 0
count needle (x:xs) = let n = if x == needle then 1 else 0 in
    n + (count needle xs)
-- count needle haystack = length ( filter (\x -> x == needle) haystack)

-- 1 == 1. && 0.1 == .1
number :: Char -> String -> [Token]
number c cs = 
    if (c == '-') && (not . isDigit $ head cs) then
        identifier c cs
    else 
        let (digs'', cs') = span isDDigit cs 
            digs' = if ((length digs'' > 0) && (head $ reverse digs'')  == '.') then c : (digs''++"0") else (c : digs'')
            digs = if ((length digs'' > 0) && c == '.') then '0':digs' else digs'
        in
        if ((count '.' digs) <= 1) && ((digs!!0 == '-') || (count '-' digs == 0))
            then TokNum (read (digs)) : tokenize cs'
            else error "Invalid number format"

