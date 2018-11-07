module Parser where
import Data.Char
import Data.Bool
import Control.Monad

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
           deriving (Show, Eq)

data Error = TokenizeError
           | NestError
           | ObjectFormatError
           deriving (Show, Eq)

toJson :: String -> Either Error JValue
toJson str = do
  tokens <- tokenizeStart str
  nest <- nest tokens
  parseNest nest
-- A parser is a tuple of a JValue and the remaining string.
-- We want a way to recursively parse, then return and continue the parse with
-- the actual string left: 
-- (JString k):(drop n cs)
-- where k = takeWhile isChar cs
--       n = length k
-- from: https://jameshfisher.com/2018/03/09/writing-a-parser-in-haskell.html
tokenizeStart :: String -> Either Error [Token]
tokenizeStart str = tokenize (filter (not . isWhitespace) str) []
tokenize :: String -> [Token] -> Either Error [Token]
tokenize [] [] = Left TokenizeError
tokenize [] ts = Right ts
tokenize cs ts =
  let ret = singleToken cs in
    case ret of
      Left err -> Left err
      Right (token, str) -> tokenize str (ts ++ [token])
      
singleToken :: String -> Either Error (Token, String)
singleToken (x:xs) | x == '{' = Right $ (LBrace, xs)
                   | x == '}' = Right $ (RBrace,  xs)
                   | x == '[' = Right $ (LBracket, xs)
                   | x == ']' = Right $ (RBracket, xs)
                   | x == ':' = Right $ (TColon, xs)
                   | x == ',' = Right $ (TComma, xs)
                   | x == '"' = Right $ (TString (takeWhile (/= '"') xs), (tail $ dropWhile (/= '"') xs)) -- the tail is to skip the closing quotation mark
                | isDigit x = Right $ (TInt (read (x:takeWhile isDigit xs)), (dropWhile isDigit xs))
                | otherwise = Left TokenizeError

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

nest :: [Token] -> Either Error N
nest tokens =
  let (ns, ts) = nestMany [] tokens
      nestLen = length ns
      tokenLen = length ts
  in
    if (nestLen > 1 || tokenLen > 0)
    then Left NestError
    else Right $ head ns

parseNest :: N -> Either Error JValue
parseNest (NString str) = Right $ JString str
parseNest (NInt i) = Right $ JNumber $ fromIntegral i
parseNest (NObject ns) =
  let ts = objectTuples ns in
    case ts of
      Left err -> Left err
      Right tuples -> Right $ JObject tuples
parseNest (NArray ns) =
  let ts = array ns in
    case ts of
      Left err -> Left err
      Right tuples -> Right $ JArray tuples

objectTuples :: [N] -> Either Error [(String, JValue)]
objectTuples [] = Right []
objectTuples ((NString str):NColon:n:ns) =
  let moreTuples = objectTuples ns in
    case moreTuples of
      Left str -> Left str
      Right ts ->
        let val = parseNest n in
          case val of
            Left err -> Left err
            Right vs -> Right $ (str, vs) : ts
            
objectTuples (NComma:ns) = objectTuples ns
objectTuples _ = Left ObjectFormatError

array :: [N] -> Either Error [JValue]
array ns = monadicArray parseNest (filter (/= NComma) ns) []

isWhitespace :: Char -> Bool
isWhitespace x | x == ' '  = True
               | x == '\n' = True
               | x == '\r' = True
               | x == '\t' = True
               | otherwise = False

monadicArray :: (a -> Either b c) -> [a] -> [c] -> Either b [c]
monadicArray f [] [] = Right []
monadicArray f [] cs = Right cs
monadicArray f (a:as) cs =
  let ans = f a in
    case ans of
      Left s -> Left s
      Right t -> monadicArray f as (cs ++ [t])
