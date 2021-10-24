module Lexer (Token (..), tokenize) where

import Data.Char (ord)

data Token
    = Comment
    | Empty
    | LiteralRational Integer Integer Integer
    | LiteralInteger Integer Integer
    | LiteralString String
    | Operator String
    | Separator Char
    | Word String
    deriving (Show, Read, Eq)

between :: (Ord t) => t -> t -> t -> Bool
between min max value = value <= max && value >= min

parseDigit :: Char -> Integer
parseDigit c = inRange '0' '9' 0 $ inRange 'A' 'Z' 10 $ inRange 'a' 'z' 10 36
    where inRange min max add elsVal = if between min max c then toInteger $ ord c - ord min + add else elsVal

isDigit = between '0' '9'
isAlpha c = between 'A' 'Z' c || between 'a' 'z' c || c == '_'
isAlnum c = isAlpha c || isDigit c
isOperator = (`elem` "+-*/:\\&|~^<>=!")
isSeparator = (`elem` "()[]")

startToken :: Char -> Token
startToken char
    | char == '"' = LiteralString ""
    | char == ';' = Comment
    | isDigit char = LiteralInteger 10 $ parseDigit char
    | isOperator char = Operator [char]
    | isSeparator char = Separator char
    | isAlpha char = Word [char]
    | otherwise = Empty

processToken :: [Token] -> Char -> [Token]
processToken tokens char = case tokens of
    Comment : rest | char == '\n' -> Empty : rest
    Comment : rest -> Comment : rest
    Empty : LiteralString s : rest | char == '"' -> LiteralString ('"' : reverse s) : rest
    LiteralString s : rest | char == '"' -> Empty : LiteralString (reverse s) : rest
    LiteralString s : rest -> LiteralString (char : s) : rest
    LiteralInteger radix n : rest | parseDigit char < radix -> LiteralInteger radix (n * radix + parseDigit char) : rest
    LiteralInteger radix n : rest | char == '.' -> LiteralRational radix n 0 : rest
    LiteralInteger 10 n : rest | (n /= 10) && (char == 'r') -> LiteralInteger n 0 : rest
    LiteralRational radix n exp : rest | parseDigit char < radix -> LiteralRational radix (radix * n + parseDigit char) (exp + 1) : rest
    Word name : rest | isAlnum char -> Word (name ++ [char]) : rest
    Operator s : rest | isOperator char -> Operator (s ++ [char]) : rest
    Empty : rest -> startToken char : rest
    tokens -> startToken char : tokens

tokenize :: String -> [Token]
tokenize = reverse . dropWhile (`elem` [Empty, Comment]) . foldl processToken []
