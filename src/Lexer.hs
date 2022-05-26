module Lexer (Token (..), tokenize) where

import Data.Char (ord)
import Data.Foldable (foldlM)
import Fallible (Fallible, err)

data Token
  = Comment
  | Empty
  | LiteralReal Integer Integer Integer
  | LiteralInt Integer Integer
  | LiteralText String Bool
  | LiteralBool Bool
  | Operator String
  | Separator Char
  | Name String deriving (Show, Eq)

between :: Ord t => t -> t -> t -> Bool
between min max value = value <= max && value >= min

parseDigit :: Char -> Integer
parseDigit c = inRange '0' '9' 0 $ inRange 'A' 'Z' 10 $ inRange 'a' 'z' 10 36
  where inRange min max add elsVal = if between min max c then toInteger $ ord c - ord min + add else elsVal

isDigit radix c = let n = parseDigit c in n < 36 && n < radix
isAlpha c = between 'A' 'Z' c || between 'a' 'z' c || c == '_'
isAlnum c = isAlpha c || isDigit 10 c
isOperator = (`elem` "+-*/%&|~^<>=!")
isSeparator = (`elem` "()[]{}:.")
isWhite = (`elem` " \t\n\r\f\v,;")

startToken :: Integer -> Char -> Fallible (Integer, Token)
startToken line '"' = Right (line, LiteralText "" False)
startToken line '#' = Right (line, Comment)
startToken line char
  | isDigit 10 char = Right (line, LiteralInt 10 $ parseDigit char)
  | isOperator char = Right (line, Operator [char])
  | isSeparator char = Right (line, Separator char)
  | isAlpha char = Right (line, Name [char])
  | isWhite char = Right (line, Empty)
  | otherwise = err line $ "Invalid character: " ++ [char]

afterInt :: Integer -> Integer -> Integer -> Integer -> Char -> Fallible [(Integer, Token)]
afterInt _ line radix n '.' = Right [(line, LiteralReal radix n 0)]
afterInt _ line radix n 'b' = if between 0 1 n then Right [(line, LiteralBool $ n == 1)] else err line $ "Invalid bool value " ++ show n
afterInt _ line radix n 'r' = if radix == 10 then Right [(line, LiteralInt n 0)] else err line "Specifying radix twice"
afterInt newLine line radix n c
  | isDigit radix c = Right [(line, LiteralInt radix $ n * radix + parseDigit c)]
  | otherwise = fmap (: [(line, LiteralInt radix n)]) $ startToken newLine c

buildToken :: [(Integer, Token)] -> Char -> Fallible [(Integer, Token)]
buildToken tokens@((line, _) : _) char = case tokens of
  (_, Comment) : rest -> Right $ (line', if char == '\n' then Empty else Comment) : rest
  (_, LiteralText s True) : rest | char == '"' -> Right $ (line, LiteralText ('"' : reverse s) False) : rest
  (_, LiteralText s False) : rest | char == '"' -> Right $ (line, LiteralText (reverse s) True) : rest
  (_, LiteralText s False) : rest -> Right $ (line', LiteralText (char : s) False) : rest
  (_, LiteralInt radix n) : rest -> fmap (++ rest) $ afterInt line' line radix n char
  (_, LiteralReal radix n exp) : rest | isDigit radix char -> Right $ (line', LiteralReal radix (radix * n + parseDigit char) $ exp + 1) : rest
  (_, Name name) : rest | isAlnum char -> Right $ (line', Name $ name ++ [char]) : rest
  (_, Operator s) : rest | isOperator char -> Right $ (line', Operator $ s ++ [char]) : rest
  (_, Empty) : rest | not (null rest) -> fmap (: rest) $ startToken line' char
  tokens -> fmap (: tokens) $ startToken line' char
  where line' = line + if char == '\n' then 1 else 0

tokenize :: String -> Fallible [(Integer, Token)]
tokenize = fmap (tail . reverse . dropWhile ((`elem` [Empty, Comment]) . snd)) . foldlM buildToken [(0, Empty)]
