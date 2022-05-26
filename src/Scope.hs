module Scope (Scope, scope, empty, addGlobal, addNamespace, addConstant, addVariable, setVariable, get) where

import qualified Data.Map.Strict as Map
import qualified Parser
import qualified Language as Lang
import Fallible (Fallible (..), err)

data Mutability = Constant | Variable deriving (Eq, Show)
data Entry = Local Mutability Lang.Type | Global Parser.Visibility Lang.Type | Namespace Parser.Visibility Scope deriving (Eq, Show)
data Scope = Scope String (Map.Map String Entry) deriving (Eq, Show)

scope :: String -> Map.Map String Entry -> Scope
scope = Scope

empty :: String -> Scope
empty = (`Scope` Map.map (Global Parser.Private) Lang.types)

getWith :: Bool -> Integer -> [String] -> Scope -> Fallible (Lang.Type, String, String, [String])
getWith onlyExported line (name : names) (Scope path scope) = case Map.lookup name scope of
  Just (Namespace Parser.Private _) | onlyExported -> err line $ "File '" ++ path ++ "' does not export " ++ name
  Just (Global Parser.Private _) | onlyExported -> err line $ "File '" ++ path ++ "' does not export " ++ name
  Nothing -> err line $ "File '" ++ path ++ "' does not contain " ++ name
  Just (Namespace _ _) | null names -> err line $ "Identifier '" ++ name ++ "' denotes a namespace, not a value"
  Just (Namespace _ scope') -> getWith True line names scope'
  Just (Global _ type') -> Right (type', name, path, names)
  Just (Local _ type') -> Right (type', name, path, names)

get :: Integer -> [String] -> Scope -> Fallible (Lang.Type, String, String, [String])
get = getWith False

add :: Integer -> Bool -> String -> Entry -> Scope -> Fallible (Scope, Bool)
add line assign name entry (Scope path scope) = case Map.lookup name scope of
  Nothing -> Right (Scope path $ Map.insert name entry scope, True)
  Just previous@(Local Variable _) | entry == previous && assign -> Right (Scope path scope, False)
  Just _ -> err line $ "Redefinition of " ++ name

addGlobal :: Integer -> Parser.Visibility -> String -> Lang.Type -> Scope -> Fallible Scope
addGlobal line visibility name type' = fmap fst . add line False name (Global visibility type')

addNamespace :: Integer -> Parser.Visibility -> String -> Scope -> Scope -> Fallible Scope
addNamespace line visibility name subscope = fmap fst . add line False name (Namespace visibility subscope)

addConstant :: Integer -> String -> Lang.Type -> Scope -> Fallible Scope
addConstant line name type' = fmap fst . add line False name (Local Constant type')

addVariable :: Integer -> String -> Lang.Type -> Scope -> Fallible Scope
addVariable line name type' = fmap fst . add line False name (Local Variable type')

setVariable :: Integer -> String -> Lang.Type -> Scope -> Fallible (Scope, Bool)
setVariable line name type' = add line True name (Local Variable type')