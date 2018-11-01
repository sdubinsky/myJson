module Parser where
import Data.Char

data JValue = JObject [(String, JValue)]
            | JArray [JValue]
            | JString String
            | JNumber Double
            deriving (Show, Eq)

data Token = LBracket
           | RBracket
           | LBrace
           | RBrace
           | TColon
           | TComma
           | TString String
           | TInt Int
           deriving (Show)


toJson :: String -> JValue
toJson str = parseNest $ nest $ tokenize str


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
                | x == ',' = TComma : tokenize xs
                | isWhitespace x = tokenize xs
                | x == '"' = TString (takeWhile (/= '"') xs) : tokenize (tail $ dropWhile (/= '"') xs) -- the tail is to skip the closing quotation mark
                | isDigit x = TInt (read (x:takeWhile isDigit xs)) : tokenize (dropWhile isDigit xs)
                | otherwise = error "tokenize error"

-- If your thing is recursive, recurse with nestMany.  If not, just create that one
-- bottom-level nest and return the rest of the tokens.

data N = NArray [N]
       | NObject [N]
       | NString String
       | NInt Int
       | NColon
       | NComma
       deriving (Show, Eq)

nestOne :: [Token] -> ([N], [Token])
nestOne [] = ([], [])
nestOne (LBrace:ts) =
  let (ns, ts') = nestMany [] ts
  in ([NObject ns], ts')
nestOne (LBracket:ts) =
  let (ns, ts') = nestMany [] ts
  in ([NArray ns], ts')

nestOne (TString s:ts) = ([NString s], ts)
nestOne (TInt i:ts) = ([NInt i], ts)
nestOne (RBrace:ts) = ([], ts)
nestOne (RBracket:ts) = ([], ts)
nestOne (TComma:ts) = ([NComma], ts)
nestOne (TColon:ts) = ([NColon], ts)
nestMany :: [N] -> [Token] -> ([N], [Token])
nestMany prev ts =
  case nestOne ts of
    ([], ts') -> (prev, ts')
    (ns, ts') -> nestMany (prev ++ ns) ts'

nest :: [Token] -> N
nest (tokens) =
  let (ns, ts) = nestMany [] tokens
      nestLen = length ns
      tokenLen = length ts
  in
    if (nestLen > 1 || tokenLen > 0)
    then error "nesting parse error"
    else head ns

parseNest :: N -> JValue
parseNest (NString str) = JString str
parseNest (NInt i) = JNumber $ fromIntegral i
parseNest (NObject ns) = JObject $ objectTuples ns
parseNest (NArray ns) = JArray $ array ns

objectTuples :: [N] -> [(String, JValue)]
objectTuples [] = []
objectTuples ((NString str):NColon:n:ns) =
  (str, parseNest n) : objectTuples ns
objectTuples (NComma:ns) = objectTuples ns
objectTuples _ = error "bad object format"

array :: [N] -> [JValue]
array ns = map parseNest $ filter (/= NComma) ns
isWhitespace :: Char -> Bool
isWhitespace x | x == ' '  = True
               | x == '\n' = True
               | x == '\r' = True
               | x == '\t' = True
               | otherwise = False

