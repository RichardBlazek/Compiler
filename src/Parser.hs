module Parser (Value (..), Statement (..), Declaration (..), Literal (..), File (..), Visibility (..), parse) where

import qualified Lexer
import qualified Data.Map.Strict as Map
import Functions (intercalate, tailRecM, tailRec2M, fmap2, after)
import Fallible (Fallible (..), err, assert)

data Visibility = Exported | Private deriving (Eq, Show)
newtype File = File [(Integer, Visibility, Declaration)] deriving (Show, Eq)

data Declaration
  = Declaration String (Integer, Value)
  | Import String String
  | Php String String (Map.Map String (Integer, Value)) deriving (Show, Eq)

data Statement
  = Value (Integer, Value)
  | Assignment String (Integer, Value)
  | IfChain [((Integer, Value), [(Integer, Statement)])] [(Integer, Statement)]
  | While (Integer, Value) [(Integer, Statement)]
  | For String (Maybe String) (Integer, Value) [(Integer, Statement)]
  | Return (Integer, Value) deriving (Show, Eq)

data Literal = Int Integer | Real Double | Text String | Bool Bool deriving (Show, Eq)

data Value
  = Literal Literal
  | Record (Map.Map String (Integer, Value))
  | Name [String]
  | Call (Integer, Value) [(Integer, Value)]
  | Operation String [(Integer, Value)]
  | Access (Integer, Value) String
  | Lambda [(String, (Integer, Value))] (Integer, Value) [(Integer, Statement)] deriving (Show, Eq)

expect :: Lexer.Token -> [(Integer, Lexer.Token)] -> Fallible [(Integer, Lexer.Token)]
expect e [] = err (-1) $ "Expected " ++ show e ++ ", reached the EOF"
expect e ((line, l) : ts) = if l == e then return ts else err line $ "Expected " ++ show e ++ ", got " ++ show l

parseMany :: Show a => ([(Integer, Lexer.Token)] -> Fallible (a, [(Integer, Lexer.Token)])) -> Lexer.Token -> [(Integer, Lexer.Token)] -> Fallible ([a], [(Integer, Lexer.Token)])
parseMany parser end tokens = tailRec2M if' reverse tail else' [] tokens
  where if' result tokens = assert (not $ null tokens) (-1) "Unexpected end of file" >> Right (snd (head tokens) == end)
        else' result tokens = fmap2 (: result) id $ parser tokens

parseFactor :: [(Integer, Lexer.Token)] -> Fallible ((Integer, Value), [(Integer, Lexer.Token)])
parseFactor ((line, Lexer.LiteralInt _ lit) : tokens) = Right ((line, Literal $ Int lit), tokens)
parseFactor ((line, Lexer.LiteralReal _ n exp) : tokens) = Right ((line, Literal $ Real $ fromIntegral n / (10.0 ** fromIntegral exp)), tokens)
parseFactor ((line, Lexer.LiteralText lit _) : tokens) = Right ((line, Literal $ Text lit), tokens)
parseFactor ((line, Lexer.LiteralBool lit) : tokens) = Right ((line, Literal $ Bool lit), tokens)
parseFactor ((line, Lexer.Name "fun") : tokens) = parseLambda line tokens
parseFactor ((line, Lexer.Name name) : tokens) = Right ((line, Name [name]), tokens)
parseFactor ((line, Lexer.Separator '(') : tokens@((_, Lexer.Name _) : (_, Lexer.Separator ':') : _)) = parseRecord line tokens
parseFactor ((line, Lexer.Separator '(') : tokens) = do
  (value, tokens) <- parseValue tokens
  fmap ((,) value) $ expect (Lexer.Separator ')') tokens
parseFactor ((line, token) : _) = err line $ "Expected a value but got " ++ show token

parseFields :: Map.Map String (Integer, Value) -> [(Integer, Lexer.Token)] -> Fallible (Map.Map String (Integer, Value), [(Integer, Lexer.Token)])
parseFields fields ((line, Lexer.Name name) : tokens) = do
  assert (Map.notMember name fields) line $ "Duplicate field " ++ name
  tokens <- expect (Lexer.Separator ':') tokens
  (value, tokens) <- parseValue tokens
  parseFields (Map.insert name value fields) tokens
parseFields fields ((_, Lexer.Separator ')') : tokens) = Right (fields, tokens)
parseFields fields ((line, token) : _) = err line $ "Expected a field name but got " ++ show token

parseRecord :: Integer -> [(Integer, Lexer.Token)] -> Fallible ((Integer, Value), [(Integer, Lexer.Token)])
parseRecord line = fmap2 ((,) line . Record) id . parseFields Map.empty

parseArguments :: [(Integer, Lexer.Token)] -> Fallible ([(String, (Integer, Value))], [(Integer, Lexer.Token)])
parseArguments tokens = tailRecM if' then' else' ([], [], tokens)
  where if' (_, _, tokens) = assert (not $ null tokens) (-1) "Unexpected end of file" >> Right (snd (head tokens) == Lexer.Separator ']')
        then' (names, args, (line, _) : tokens) = assert (null names) line ("Missing type for " ++ intercalate ", " names) >> Right (reverse args, tokens)
        else' (names, args, (_, Lexer.Name name) : tokens) = Right (name : names, args, tokens)
        else' (names, args, (_, Lexer.Separator ':') : tokens) = do
          (type', tokens) <- parseValue tokens
          Right ([], map (flip (,) type') names ++ args, tokens)
        else' (names, args, (line, token) : _) = err line $ "Unexpected " ++ show token ++ " in the argument list"

parseLambda :: Integer -> [(Integer, Lexer.Token)] -> Fallible ((Integer, Value), [(Integer, Lexer.Token)])
parseLambda line tokens = do
  ((args, (returnType, block)), tokens'') <- expect (Lexer.Separator '[') tokens >>= after parseArguments (after parseValue parseBlock)
  Right ((line, Lambda args returnType block), tokens'')

parseCall :: [(Integer, Lexer.Token)] -> Fallible ((Integer, Value), [(Integer, Lexer.Token)])
parseCall tokens = parseFactor tokens >>= uncurry (tailRec2M if' id id else')
  where if' fun tokens = Right $ null tokens || snd (head tokens) `notElem` [Lexer.Separator '[', Lexer.Separator '.']
        else' fun ((line, Lexer.Separator '[') : tokens) = fmap2 ((,) line . Call fun) id $ parseMany parseValue (Lexer.Separator ']') tokens
        else' (line, Name names) ((_, Lexer.Separator '.') : (_, Lexer.Name name) : tokens) = Right ((line, Name $ name : names), tokens)
        else' obj ((line, Lexer.Separator '.') : (_, Lexer.Name name) : tokens) = Right ((line, Access obj name), tokens)
        else' obj ((line, Lexer.Separator c) : _) = err line $ "Expected field or builtin name after " ++ [c]

parseValue :: [(Integer, Lexer.Token)] -> Fallible ((Integer, Value), [(Integer, Lexer.Token)])
parseValue tokens = parseCall tokens >>= uncurry (tailRec2M if' id id else')
  where if' first tokens = Right (case tokens of (_, Lexer.Operator _) : _ -> False; _ -> True)
        else' first ((line, Lexer.Operator op) : tokens) = fmap2 (\second -> (line, Operation op [first, second])) id $ parseCall tokens

parseIf :: Integer -> [((Integer, Value), [(Integer, Statement)])] -> [(Integer, Lexer.Token)] -> Fallible ((Integer, Statement), [(Integer, Lexer.Token)])
parseIf line chain tokens = do
  (pair, tokens) <- after parseValue parseBlock tokens
  let chain' = pair : chain
  case tokens of
    (_, Lexer.Name "else") : (_, Lexer.Name "if") : tokens -> parseIf line chain' tokens
    (_, Lexer.Name "else") : tokens -> fmap2 (((,) line) . IfChain (reverse chain')) id $ parseBlock tokens
    _ -> Right ((line, IfChain (reverse chain') []), tokens)

parseBlock :: [(Integer, Lexer.Token)] -> Fallible ([(Integer, Statement)], [(Integer, Lexer.Token)])
parseBlock tokens = expect (Lexer.Separator '{') tokens >>= parseMany parseStatement (Lexer.Separator '}')

parseBlockWith :: Integer -> ((Integer, Value) -> [(Integer, Statement)] -> a) -> [(Integer, Lexer.Token)] -> Fallible ((Integer, a), [(Integer, Lexer.Token)])
parseBlockWith line fn tokens = fmap2 ((,) line . uncurry fn) id $ after parseValue parseBlock tokens

parseStatement :: [(Integer, Lexer.Token)] -> Fallible ((Integer, Statement), [(Integer, Lexer.Token)])
parseStatement ((line, Lexer.Name name) : (_, Lexer.Operator "=") : tokens) = fmap2 (((,) line) . Assignment name) id $ parseValue tokens
parseStatement ((line, Lexer.Name "if") : tokens) = parseIf line [] tokens
parseStatement ((line, Lexer.Name "while") : tokens) = parseBlockWith line While tokens
parseStatement ((line, Lexer.Name "for") : (_, Lexer.Name value) : (_, Lexer.Separator ':') : tokens) = parseBlockWith line (For value Nothing) tokens
parseStatement ((line, Lexer.Name "for") : (_, Lexer.Name key) : (_, Lexer.Name value) : (_, Lexer.Separator ':') : tokens) = parseBlockWith line (For value $ Just key) tokens
parseStatement ((line, Lexer.Name "return") : tokens) = fmap2 ((,) line . Return) id $ parseValue tokens
parseStatement tokens@((line, _):_) = fmap2 ((,) line . Value) id $ parseValue tokens

parseDeclaration :: (String -> String) -> [(Integer, Lexer.Token)] -> Fallible ((Integer, Visibility, Declaration), [(Integer, Lexer.Token)])
parseDeclaration convertPath tokens@((line, Lexer.Name name) : _) = do
  (tokens, export) <- return $ if name == "export" then (tail tokens, Exported) else (tokens, Private)
  fmap2 ((,,) line export) id $ case tokens of
    (_, Lexer.Name "import") : (_, Lexer.Name name) : (_, Lexer.LiteralText path _) : tokens -> Right (Import name $ convertPath path, tokens)
    (_, Lexer.Name "import") : (_, Lexer.Name "php") : (_, Lexer.Name name) : (_, Lexer.LiteralText path _) : tokens -> do
      tokens <- expect (Lexer.Separator '(') tokens
      (imports, tokens) <- parseFields Map.empty tokens
      Right (Php name (convertPath path) imports, tokens)
    (_, Lexer.Name name) : tokens -> do
      tokens <- expect (Lexer.Operator "=") tokens
      (value, tokens) <- parseValue tokens
      Right (Declaration name value, tokens)
    (_, token) : _ -> err line $ "Expected a name to export but got " ++ show token

parseDeclaration convertPath ((line, token) : _) = err line $ "Expected a name of declared function but got " ++ show token

parse :: (String -> String) -> String -> Fallible File
parse convertPath input = Lexer.tokenize input >>= tailRecM (Right . null . snd) (Right . File . reverse . fst) else' . (,) []
  where else' (result, tokens) = fmap2 (: result) id $ parseDeclaration convertPath tokens
