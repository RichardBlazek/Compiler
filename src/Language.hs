module Language (Type (..), Builtin (..), builtinCall, fieldAccess, removeBuiltins, types) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Fallible (Fallible (..), err, assert, assertJust)
import Functions (join, split, swapMap, (??), zipMaps, intercalate, listify)

data Type = Void | Int | Bool | Real | Text | Db | Function [Type] Type | Dictionary Type Type | Vector Type | Record (Map.Map String Type) deriving (Eq, Ord)
data Builtin = Add | Sub | Mul | Div | IntDiv | Rem | And | Or | Xor | Eq | Neq | Lt | Gt | Le | Ge | Pow | Not | AsInt | AsReal | AsBool | AsText
  | Fun | Dict | List | Set | Get | Has | Count | Concat | Pad | Sort | Join | Insert | Erase | Append | Remove | Find | AsList | AsDict | Map
  | Filter | Fold | Keys | Values | Flat | Shuffle | Split | EscapeHtml | UnescapeHtml | EscapeUrl | UnescapeUrl | Replace | Hash | IsHashOf
  | StartsWith | EndsWith | Contains | Lower | Upper | Trim | Connect | Connected | Query deriving (Eq, Ord)

instance Show Type where
  show (Function ts t) = show t ++ ".fun[" ++ intercalate ", " (map show ts) ++ "]" 
  show (Dictionary k v) = show v ++ ".dict[" ++ show k ++ "]"
  show (Vector t) = show t ++ ".list"
  show (Record m) = "(" ++ intercalate ", " (map (\(k, v) -> k ++ ": " ++ show v) (Map.toList m)) ++ ")"
  show type' = swapMap types Map.! type'

instance Show Builtin where
  show builtin = swapMap builtins Map.! builtin

builtins :: Map.Map String Builtin
builtins = Map.fromList [("+", Add), ("-", Sub), ("*", Mul), ("/", Div), ("//", IntDiv), ("%", Rem), ("&", And), ("|", Or), ("^", Xor), ("==", Eq), ("!=", Neq),
  ("<", Lt), (">", Gt), ("<=", Le), (">=", Ge), ("**", Pow), ("not", Not), ("asInt", AsInt), ("asReal", AsReal), ("asBool", AsBool), ("asText", AsText),
  ("fun", Fun), ("dict", Dict), ("list", List), ("set", Set), ("get", Get), ("has", Has), ("count", Count), ("concat", Concat), ("pad", Pad), ("sort", Sort),
  ("join", Join), ("insert", Insert), ("erase", Erase), ("append", Append), ("remove", Remove), ("find", Find), ("asList", AsList), ("asDict", AsDict),
  ("map", Map), ("filter", Filter), ("fold", Fold), ("keys", Keys), ("values", Values), ("flat", Flat), ("shuffle", Shuffle),
  ("split", Split), ("escapeHtml", EscapeHtml), ("unescapeHtml", UnescapeHtml), ("escapeUrl", EscapeUrl), ("unescapeUrl", UnescapeUrl), ("replace", Replace), ("hash", Hash),
  ("isHashOf", IsHashOf), ("startsWith", StartsWith), ("endsWith", EndsWith), ("contains", Contains), ("lower", Lower), ("upper", Upper),
  ("trim", Trim), ("connect", Connect), ("connected", Connected), ("query", Query)]

builtinsSet :: Set.Set String
builtinsSet = Map.keysSet builtins

types :: Map.Map String Type
types = Map.fromList [("void", Void), ("int", Int), ("bool", Bool), ("real", Real), ("text", Text), ("db", Db)]

removeBuiltins :: Set.Set String -> Set.Set String
removeBuiltins = (`Set.difference` builtinsSet)

getResultType :: Integer -> Builtin -> [Type] -> Fallible Type
getResultType line builtin args = case (builtin, args) of
  (Add, [Int, Int]) -> Right Int
  (Add, [a, b]) | all (`elem` [Int, Real]) [a, b] -> Right Real
  (Add, [Text, Text]) -> Right Text
  (Add, [Vector v1, Vector v2]) | v1 == v2 -> Right $ Vector v1
  (Sub, [Int, Int]) -> Right Int
  (Sub, [a, b]) | all (`elem` [Int, Real]) [a, b] -> Right Real
  (Mul, [Int, Int]) -> Right Int
  (Mul, [a, b]) | all (`elem` [Int, Real]) [a, b] -> Right Real
  (Div, [a, b]) | all (`elem` [Int, Real]) [a, b] -> Right Real
  (IntDiv, [a, b]) | all (`elem` [Int, Real]) [a, b] -> Right Int
  (Rem, [Int, Int]) -> Right Int
  (Rem, [a, b]) | all (`elem` [Int, Real]) [a, b] -> Right Real
  (Pow, [Int, Int]) -> Right Int
  (Pow, [a, b]) | all (`elem` [Int, Real]) [a, b] -> Right Real
  (And, [Int, Int]) -> Right Int
  (And, [Bool, Bool]) -> Right Bool
  (And, [Dictionary k1 v1, Dictionary k2 v2]) | v1 == v2 && k1 == k2 -> Right $ Dictionary k1 v1
  (And, [Record f1, Record f2]) -> Right $ Record $ Map.intersection f2 f1
  (Or, [Int, Int]) -> Right Int
  (Or, [Bool, Bool]) -> Right Bool
  (Or, [Dictionary k1 v1, Dictionary k2 v2]) | v1 == v2 && k1 == k2 -> Right $ Dictionary k1 v1
  (Or, [Record f1, Record f2]) -> Right $ Record $ Map.union f2 f1
  (Xor, [Int, Int]) -> Right Int
  (Xor, [Bool, Bool]) -> Right Bool
  (Eq, [Bool, Bool]) -> Right Bool
  (Eq, [Text, Text]) -> Right Bool
  (Eq, [a, b]) | all (`elem` [Int, Real]) [a, b] -> Right Bool
  (Eq, [Vector v1, Vector v2]) -> getResultType line Eq [v1, v2]
  (Eq, [Dictionary k1 v1, Dictionary k2 v2]) -> getResultType line Eq [k1, k2] >> getResultType line Eq [v1, v2]
  (Eq, [Record f1, Record f2]) -> do
    mapM (getResultType line Eq) $ map (\(a, b) -> [a, b]) $ Map.elems $ zipMaps f1 f2
    Right Bool
  (Neq, compared) -> getResultType line Eq compared
  (Lt, [a, b]) | all (`elem` [Int, Real]) [a, b] -> Right Bool
  (Lt, [Text, Text]) -> Right Bool
  (Gt, compared) -> getResultType line Lt compared
  (Le, compared) -> getResultType line Lt compared
  (Ge, compared) -> getResultType line Lt compared
  (Not, [Int]) -> Right Int
  (Not, [Bool]) -> Right Bool
  (AsInt, [a]) | a `elem` [Int, Real, Bool, Text] -> Right Int
  (AsReal, [a]) | a `elem` [Int, Real, Bool, Text] -> Right Real
  (AsBool, [a]) | a `elem` [Int, Real, Bool, Text] -> Right Bool
  (AsText, [a]) | a `elem` [Int, Real, Bool, Text] -> Right Text
  (AsText, [Vector v]) -> getResultType line AsText [v]
  (AsText, [Dictionary k v]) -> getResultType line AsText [v]
  (AsText, [Record fields]) -> mapM (getResultType line AsText . listify) (Map.elems fields) >> Right Text
  (Fun, returned : args) -> Right $ Function args returned
  (List, v : args) -> do
    assert (all (== v) args) line "All values must have the specified type"
    Right $ Vector v
  (Dict, v : k : args) -> do
    assert (k `elem` [Int, Text]) line "Map keys must be either int or text"
    let (pairs, leftover) = split args
    assert (leftover == Nothing) line "Missing value for the last key"
    assert (all (== (k, v)) pairs) line "All keys and values must have the specified type"
    Right $ Dictionary k v
  (Get, [Vector v, Int]) -> Right v
  (Get, [Vector v, Int, Int]) -> Right $ Vector v
  (Get, [Dictionary k v, k2]) | k == k2 -> Right v
  (Set, [Vector v, Int, assigned]) | assigned == v -> Right Void
  (Set, [Dictionary k v, k2, assigned]) | k == k2 && assigned == v -> Right Void
  (Has, [Dictionary k v, k2]) | k == k2 -> Right Bool
  (Count, [Text]) -> Right Int
  (Count, [Vector _]) -> Right Int
  (Count, [Dictionary _ _]) -> Right Int
  (Count, [Text, Function [Text] Bool]) -> Right Int
  (Count, [Vector v, Function [v2] Bool]) | v == v2 -> Right Int
  (Count, [Dictionary _ v, Function [v2] Bool]) | v == v2 -> Right Int
  (Concat, Vector v : args) | all (`elem` [Vector v, v]) args -> Right $ Vector v
  (Pad, [Vector v, Int]) -> Right $ Vector v
  (Pad, [Vector v, Int, v2]) | v2 == v -> Right $ Vector v
  (Pad, [Vector v, Int, v2, Bool]) | v2 == v -> Right $ Vector v
  (Pad, [Text, Int]) -> Right Text
  (Pad, [Text, Int, Text]) -> Right Text
  (Pad, [Text, Int, Text, Bool]) -> Right Text
  (Sort, Vector v : args) | args `elem` [[], [Bool], [Function [v, v] Int]] -> Right Void
  (Join, [Vector v]) -> getResultType line AsText [v]
  (Join, [Vector v, Text]) -> getResultType line AsText [v]
  (Insert, [Vector v, Int, v2]) | v == v2 -> Right Void
  (Insert, [Vector v, Int, Vector v2]) | v == v2 -> Right Void
  (Erase, [Vector v, Int]) -> Right Void
  (Erase, [Vector v, Int, Int]) -> Right Void
  (Erase, [Dictionary k _, k2]) | k == k2 -> Right Void
  (Append, Vector v : args) | all (`elem` [Vector v, v]) args -> Right Void
  (Remove, [Vector v, v2]) | v == v2 -> Right Void
  (Remove, [Dictionary k v, v2]) | v == v2 -> Right Void
  (Find, [Vector v, v2]) | v == v2 -> Right Int
  (Find, [Dictionary k v, v2]) | v == v2 -> Right k
  (AsList, [Text]) -> Right $ Vector Text
  (AsList, [Dictionary k v]) -> Right $ Vector $ Record $ Map.fromList [("key", k), ("value", v)]
  (AsDict, [Vector v]) -> do
    let fieldsOf (Record fields) = Right fields
        fieldsOf type' = err line $ "Expected a record, got " ++ show type'
    fields <- fieldsOf v
    key <- assertJust (Map.lookup "key" fields) line "Missing key field"
    value <- assertJust (Map.lookup "value" fields) line "Missing value field"
    getResultType line Dict [key, value]
  (Map, (Function args returnType : collections@(_ : _))) -> do
    let itemType Text = Right Text
        itemType (Vector v) = Right v
        itemType (Dictionary _ v) = Right v
        itemType type' = err line $ "Expected a list or dict, got: " ++ show type'
    types <- mapM itemType collections
    assert (args == types) line $ "Values of lists must have types " ++ join ", " args
    Right $ Vector returnType
  (Filter, [Text, Function [Text] Bool]) -> Right Text
  (Filter, [Vector v, Function [v2] Bool]) | v == v2 -> Right $ Vector v
  (Filter, [Dictionary k v, Function [v2] Bool]) | v == v2 -> Right $ Dictionary k v
  (Fold, [Text, seed, Function [s2, Text] s3]) | seed == s2 && s2 == s3 -> Right seed
  (Fold, [Vector v, seed, Function [s2, v2] s3]) | seed == s2 && s2 == s3 && v == v2 -> Right seed
  (Fold, [Dictionary k v, seed, Function [s2, v2] s3]) | seed == s2 && s2 == s3 && v == v2 -> Right seed
  (Keys, [Vector v]) -> Right $ Vector Int
  (Keys, [Dictionary k v]) -> Right $ Vector k
  (Values, [Vector v]) -> Right $ Vector v
  (Values, [Dictionary k v]) -> Right $ Vector v
  (Flat, [Vector (Vector v)]) -> Right $ Vector v
  (Shuffle, [Vector v]) -> Right $ Vector v
  (Contains, [Text, Text]) -> Right Bool
  (Contains, [Vector v, v2]) | v == v2 -> Right Bool
  (Contains, [Dictionary k v, v2]) | v == v2 -> Right Bool
  (Split, [Text, Text]) -> Right $ Vector Text
  (EscapeHtml, [Text]) -> Right Text
  (UnescapeHtml, [Text]) -> Right Text
  (EscapeUrl, [Text]) -> Right Text
  (UnescapeUrl, [Text]) -> Right Text
  (Replace, [Text, Text, Text]) -> Right Text
  (Hash, [Text]) -> Right Text
  (IsHashOf, [Text, Text]) -> Right Bool
  (StartsWith, [Text, Text]) -> Right Bool
  (EndsWith, [Text, Text]) -> Right Bool
  (Lower, [Text]) -> Right Text
  (Upper, [Text]) -> Right Text
  (Trim, [Text]) -> Right Text
  (Connect, [Text, Text, Text]) -> Right Db
  (Connected, [Db]) -> Right Bool
  (Query, Db : Record fields : Text : args) | all (`elem` [Text, Int, Bool, Real]) args -> Right $ Vector $ Record fields
  (Query, Db : Void : Text : args) | all (`elem` [Text, Int, Bool, Real]) args -> Right Void
  _ -> err line $ "Builtin " ++ show builtin ++ " does not accept arguments of types " ++ join ", " args

builtinCall :: Integer -> String -> [Type] -> Fallible (Builtin, Type)
builtinCall line name args = do
  builtin <- Map.lookup name builtins ?? err line ("Builtin " ++ name ++ " does not exist")
  returnType <- getResultType line builtin args
  Right (builtin, returnType)

fieldAccess :: Integer -> String -> Type -> Fallible Type
fieldAccess line name (Record fields) = Map.lookup name fields ?? err line ("The record does not have a field " ++ name)
fieldAccess line name type' = err line $ "It is " ++ show type' ++ ", not a record; therefore it cannot have a field " ++ name
