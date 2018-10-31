module Parser where
import Data.Char
data JValue = JObject [(String, JValue)]
            | JArray [JValue]
            | JString String
            | JNumber Double
            deriving (Show)

data Token = LBracket
           | RBracket
           | LBrace
           | RBrace
           | TColon
           | TString String
           | TInt Int
           deriving (Show)

-- A parser is a tuple of a JValue and the remaining string.
-- We want a way to recursively parse, then return and continue the parse with
-- the actual string left: 
-- (JString k):(drop n cs)
-- where k = takeWhile isChar cs
--       n = length k
-- from: https://jameshfisher.com/2018/03/09/writing-a-parser-in-haskell.html


tokenize :: String -> [Token]
tokenize [] = []
tokenize (x:xs) | x == '{' = LBrace : tokenize xs
                | x == '}' = RBrace : tokenize xs
                | x == '[' = LBracket : tokenize xs
                | x == ']' = RBracket : tokenize xs
                | x == ':' = TColon : tokenize xs
                | isWhitespace x = tokenize xs
                | x == '"' = TString (takeWhile (/= '"') xs) : tokenize (tail $ dropWhile (/= '"') xs) -- the tail is to skip the closing quotation mark
                | isDigit x = TInt (read (x:takeWhile isDigit xs)) : tokenize (dropWhile isDigit xs)

isWhitespace :: Char -> Bool
isWhitespace x | x == ' '  = True
               | x == '\n' = True
               | x == '\r' = True
               | x == '\t' = True
               | otherwise = False

