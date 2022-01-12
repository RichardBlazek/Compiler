module Codegen (gen) where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Language as Lang
import Semantics (Expression (..), Statement (..))
import Parser (Literal (..))
import Fallible (Fallible, failure)
import Functions (intercalate, split, number, pad)
import Data.Maybe (fromJust)

capturesOfBlock :: (String -> String) -> String -> Set.Set String -> [Statement] -> Set.Set String
capturesOfBlock _ _ _ [] = Set.empty
capturesOfBlock qual this skip (Expression value : rest) = capturesOfExpression qual this skip value
capturesOfBlock qual this skip (Initialization name value : rest) = Set.union (capturesOfExpression qual this skip value) $ capturesOfBlock qual this (Set.insert name skip) rest
capturesOfBlock qual this skip (Assignment _ value : rest) = Set.union (capturesOfExpression qual this skip value) $ capturesOfBlock qual this skip rest
capturesOfBlock qual this skip (While condition block : rest) = Set.unions [capturesOfExpression qual this skip condition, capturesOfBlock qual this skip block, capturesOfBlock qual this skip rest]
capturesOfBlock qual this skip (IfChain chains else' : rest) = Set.unions [Set.unions $ map (\(cond, block) -> Set.union (capturesOfExpression qual this skip cond) $ capturesOfBlock qual this skip block) chains, capturesOfBlock qual this skip else', capturesOfBlock qual this skip rest]

capturesOfExpression :: (String -> String) -> String -> Set.Set String -> (Lang.Type, Expression) -> Set.Set String
capturesOfExpression qual _ skip (_, Name pkg name) = Set.difference (Set.singleton $ qual pkg ++ name) skip
capturesOfExpression qual this skip (_, Lambda args body) = capturesOfBlock qual this (Set.union skip $ Set.fromList $ map ((qual this ++) . fst) args) body
capturesOfExpression qual this skip (_, Call fun args) = Set.unions $ map (capturesOfExpression qual this skip) $ fun : args
capturesOfExpression _ _ skip _ = Set.empty

defaultValue :: Lang.Type -> String
defaultValue Lang.Void = "NULL"
defaultValue Lang.Int = "0"
defaultValue Lang.Bool = "FALSE"
defaultValue Lang.Float = "0.0"
defaultValue Lang.Text = "''"
defaultValue (Lang.Function args result) = "(function(" ++ intercalate "," (map (\n -> "$_" ++ show n) $ number args) ++ "){return " ++ defaultValue result ++ ";})"
defaultValue (Lang.Vector _) = "z1array::n([])"
defaultValue (Lang.Dictionary _ _) = "z1array::n([])"
defaultValue (Lang.Record fields) = "z1array::n([" ++ intercalate "," (map genAssoc $ Map.assocs fields) ++ "])"
  where genAssoc (name, value) = "'" ++ name ++ "'=>" ++ defaultValue value

asArrayArgument :: Lang.Type -> (Lang.Type, String) -> String
asArrayArgument itemType (Lang.Vector type', arg) | type' == itemType = arg
asArrayArgument itemType (type', arg) = "[" ++ arg ++ "]"

genPrimitive :: Lang.Primitive -> [String] -> [Lang.Type] -> String
genPrimitive Lang.Add [a, b] [Lang.Int, Lang.Int] = "(int)(" ++ a ++ "+" ++ b ++ ")"
genPrimitive Lang.Add [a, b] [Lang.Text, Lang.Text] = "(" ++ a ++ "." ++ b ++ ")"
genPrimitive Lang.Add [a, b] [Lang.Vector _, Lang.Vector _] = a ++ "->concat(" ++ b ++ ")"
genPrimitive Lang.Add [a, b] [_, _] = "(" ++ a ++ "+" ++ b ++ ")"
genPrimitive Lang.Sub [a, b] [Lang.Int, Lang.Int] = "(int)(" ++ a ++ "-" ++ b ++ ")"
genPrimitive Lang.Sub [a, b] [_, _] = "(" ++ a ++ "-" ++ b ++ ")"
genPrimitive Lang.Mul [a, b] [Lang.Int, Lang.Int] = "(int)(" ++ a ++ "*" ++ b ++ ")"
genPrimitive Lang.Mul [a, b] [_, _] = "(" ++ a ++ "*" ++ b ++ ")"
genPrimitive Lang.Div [a, b] [_, _] = "(" ++ a ++ "/(float)" ++ b ++ ")"
genPrimitive Lang.IntDiv [a, b] [_, _] = "(int)(" ++ a ++ "/" ++ b ++ ")"
genPrimitive Lang.Rem [a, b] [Lang.Int, Lang.Int] = "(" ++ a ++ "%" ++ b ++ ")"
genPrimitive Lang.Rem [a, b] [_, _] = "fmod(" ++ a ++ "," ++ b ++ ")"
genPrimitive Lang.And [a, b] [Lang.Int, Lang.Int] = "(" ++ a ++ "&" ++ b ++ ")"
genPrimitive Lang.And [a, b] [_, _] = "(" ++ a ++ "&&" ++ b ++ ")"
genPrimitive Lang.Or [a, b] [Lang.Int, Lang.Int] = "(" ++ a ++ "||" ++ b ++ ")"
genPrimitive Lang.Or [a, b] [Lang.Dictionary _ _, Lang.Dictionary _ _] = a ++ "->unionD(" ++ b ++ ")"
genPrimitive Lang.Or [a, b] [Lang.Record _, Lang.Record f2] = a ++ "->unionR(" ++ b ++ ",array(" ++ intercalate "," (Map.keys f2) ++ "))"
genPrimitive Lang.Or [a, b] [Lang.Bool, Lang.Bool] = "(" ++ a ++ "||" ++ b ++ ")"
genPrimitive Lang.Xor [a, b] [Lang.Bool, Lang.Bool] = "(" ++ a ++ "!==" ++ b ++ ")"
genPrimitive Lang.Xor [a, b] [Lang.Int, Lang.Int] = "(" ++ a ++ "^" ++ b ++ ")"
genPrimitive Lang.Eq _ [a, b] | a /= b = "FALSE"
genPrimitive Lang.Eq [a, b] [_, _] = "(" ++ a ++ "==" ++ b ++ ")"
genPrimitive Lang.Neq _ [a, b] | a /= b = "FALSE"
genPrimitive Lang.Neq [a, b] [_, _] = "(" ++ a ++ "!=" ++ b ++ ")"
genPrimitive Lang.Lt [a, b] [_, _] = "(" ++ a ++ "<" ++ b ++ ")"
genPrimitive Lang.Gt [a, b] [_, _] = "(" ++ a ++ ">" ++ b ++ ")"
genPrimitive Lang.Le [a, b] [_, _] = "(" ++ a ++ "<=" ++ b ++ ")"
genPrimitive Lang.Ge [a, b] [_, _] = "(" ++ a ++ ">=" ++ b ++ ")"
genPrimitive Lang.Pow [a, b] [Lang.Int, Lang.Int] = "(int)pow(" ++ a ++ "**" ++ b ++ ")"
genPrimitive Lang.Pow [a, b] [_, _] = "pow(" ++ a ++ "," ++ b ++ ")"
genPrimitive Lang.Not [a] [Lang.Int] = "(~" ++ a ++ ")"
genPrimitive Lang.Not [a] [Lang.Bool] = "(!" ++ a ++ ")"
genPrimitive Lang.AsInt [a] [_] = "(int)" ++ a
genPrimitive Lang.AsBool [a] [Lang.Text] = "(" ++ a ++ "!=='')"
genPrimitive Lang.AsBool [a] [_] = "(bool)" ++ a
genPrimitive Lang.AsFloat [a] [_] = "(float)" ++ a
genPrimitive Lang.AsText [a] [Lang.Text] = a
genPrimitive Lang.AsText [a] [_] = "json_encode(" ++ a ++ ")"
genPrimitive Lang.Dict (_ : _ : args) _ = "z1array::n([" ++ intercalate "," (map (\(k, v) -> k ++ "=>" ++ v) $ fromJust $ split args) ++ "])"
genPrimitive Lang.List (_ : args) _ = "z1array::n([" ++ intercalate "," args ++ "])"
genPrimitive Lang.Get [array, key] [Lang.Vector _, _] = array ++ "->getV(" ++ key ++ ")"
genPrimitive Lang.Get [array, key] [Lang.Dictionary _ _, _] = array ++ "->getD(" ++ key ++ ")"
genPrimitive Lang.Set [array, key, value] [Lang.Vector _, _, _] = array ++ "->setV(" ++ key ++ "," ++ value ++ ")"
genPrimitive Lang.Set [array, key, value] [Lang.Dictionary _ _, _, _] = array ++ "->setD(" ++ key ++ "," ++ value ++ ")"
genPrimitive Lang.Has [array, key] [Lang.Dictionary _ _, _] = array ++ "->hasD(" ++ key ++ ")"
genPrimitive Lang.Size [text] [Lang.Text] = "mb_strlen(" ++ text ++ ")"
genPrimitive Lang.Size [array] [Lang.Vector _] = "count(" ++ array ++ "->a)"
genPrimitive Lang.Size [array] [Lang.Dictionary _ _] = "count(" ++ array ++ "->a)"
genPrimitive Lang.Concat (array : args) (Lang.Vector v : types) = array ++ "->concat(" ++ intercalate "," (map (asArrayArgument v) $ zip types args) ++ ")"
genPrimitive Lang.Append (array : args) (Lang.Vector v : types) = array ++ "->append(" ++ intercalate "," (map (asArrayArgument v) $ zip types args) ++ ")"
genPrimitive Lang.Sized [array, size] [Lang.Vector v, Lang.Int] = array ++ "->sized(" ++ size ++ "," ++ defaultValue v ++ ")"
genPrimitive Lang.Sized [array, size, value] [Lang.Vector _, Lang.Int, _] = array ++ "->sized(" ++ size ++ "," ++ value ++ ")"
genPrimitive Lang.Sort [array] _ = array ++ "->sort(FALSE)"
genPrimitive Lang.Sort [array, reversed] [Lang.Vector _, Lang.Bool] = array ++ "->sort(" ++ reversed ++ ")"
genPrimitive Lang.Sort [array, compare] _ = array ++ "->usort(" ++ compare ++ ")"
genPrimitive Lang.Join [array, separator] [Lang.Vector Lang.Text, Lang.Text] = "implode(" ++ separator ++ "," ++ array ++ "->a)"
genPrimitive Lang.Join [array, separator] [Lang.Vector v, Lang.Text] = "implode(" ++ separator ++ ",array_map(" ++ array ++ "->a,'json_encode'))"

genStatement :: (String -> String) -> String -> Statement -> String
genStatement qual this (Expression value) = genExpression qual this value ++ ";"
genStatement qual this (Initialization name value) = qual this ++ name ++ "=" ++ genExpression qual this value ++ ";"
genStatement qual this (Assignment name value) = qual this ++ name ++ "=" ++ genExpression qual this value ++ ";"
genStatement qual this (While condition block) = "while(" ++ genExpression qual this condition ++ "){" ++ genBlock qual this False block ++ "}"
genStatement qual this (IfChain chain else') = intercalate "else " (map genIf chain) ++ "else{" ++ genBlock qual this False else' ++ "}"
  where genIf (cond, block) = "if(" ++ genExpression qual this cond ++ "){" ++ genBlock qual this False block ++ "}"

genBlock :: (String -> String) -> String -> Bool -> [Statement] -> String
genBlock _ _ _ [] = ""
genBlock qual this True [statement@(Expression (type', _))] | type' /= Lang.Void = "return " ++ genStatement qual this statement
genBlock qual this return (statement : statements) = genStatement qual this statement ++ genBlock qual this return statements

genExpression :: (String -> String) -> String -> (Lang.Type, Expression) -> String
genExpression _ _ (_, Literal (Bool b)) = if b then "TRUE" else "FALSE"
genExpression _ _ (_, Literal (Int i)) = show i
genExpression _ _ (_, Literal (Real r)) = show r
genExpression _ _ (_, Literal (Text s)) = show s
genExpression qual this (_, Name pkg name) = qual pkg ++ name
genExpression qual this (_, Call fun args) = genExpression qual this fun ++ "(" ++ (intercalate "," $ map (genExpression qual this) args) ++ ")"
genExpression qual this (_, Primitive primitive args) = genPrimitive primitive (map (genExpression qual this) args) (map fst args)
genExpression qual this (_, Record fields) = "z1array::n([" ++ intercalate "," (map genAssoc $ Map.assocs fields) ++ "])"
  where genAssoc (name, value) = "'" ++ name ++ "'=>" ++ genExpression qual this value
genExpression qual this (Lang.Function _ returnType', Lambda args block) = header ++ "{" ++ genBlock qual this (returnType' /= Lang.Void) block ++ "})"
  where argNames = map ((qual this ++) . fst) args
        captures = intercalate "," $ Set.map (("&" ++ qual this) ++) $ Lang.removePrimitives $ capturesOfBlock qual this (Set.fromList argNames) block
        header = "(function(" ++ intercalate "," argNames ++ ")" ++ (if null captures then "" else "use(" ++ captures ++ ")")
genExpression qual this (_, Access obj field) = genExpression qual this obj ++ "[\"" ++ field ++ "\"]"

genDeclaration :: (String -> String) -> String -> String -> (Lang.Type, Semantics.Expression) -> String
genDeclaration qual this name value = qual this ++ name ++ "=" ++ genExpression qual this value ++ ";"

preamble :: [String] -> String
preamble quals = "<?php " ++ concat (map (\q -> concat $ map (q ++) ["int=0;", "float=0.0;", "bool=FALSE;", "text='';", "void=NULL;"]) quals) ++ "\
\class z1array implements JsonSerializable{\
  \public $a;\
  \public static function n($a) {\
    \$z=new z1array();\
    \$z->a=$a;\
    \return $z;\
  \}\
  \public function getV($i){\
    \if($i<0||$i>=count($this->a)){\
      \die('There is no element at '.$i);\
    \}\
    \return $this->a[$i];\
  \}\
  \public function setV($i,$v){\
    \$c=count($this->a);\
    \if($i<0){\
      \die('Assigning to a negative index '.$i);\
    \}\
    \if($i>=count($this->a)){\
      \$this->a=array_pad($this->a,$i+1,$v);\
    \}else{\
      \$this->a[$i]=$v;\
    \}\
  \}\
  \public function getD($i){\
    \if(!array_key_exists($i,$this->a)){\
      \die('Map does not contain the key '.$i);\
    \}\
    \return $this->a[$i];\
  \}\
  \public function setD($i,$v){\
    \$this->a[$i]=$v;\
  \}\
  \public function hasD($k){\
    \return array_key_exists($k,$this->a);\
  \}\
  \public function unionD($x){\
    \return z1array::n($x->a+$this->a);\
  \}\
  \public function concat(...$a){\
    \return z1array::n(array_merge($this->a, ...$a));\
  \}\
  \public function append(...$a){\
    \$this->a=array_merge($this->a, ...$a);\
  \}\
  \public function unionR($x,$k){\
    \return z1array::n(array_intersect_key($x->a,$k)+$this->a);\
  \}\
  \public function sized($n,$v){\
    \if($n<0){\
      \die('Array size must be non-negative, got '.$n);\
    \}\
    \if($n<count($this->a)){\
      \return z1array::n(array_slice($this->a,0,$n));\
    \}\
    \return z1array::n(array_pad($this->a,$n,$v));\
  \}\
  \public function sort($r){\
    \if($r){\
      \rsort($this->a);\
    \}else{\
      \sort($this->a);\
    \}\
  \}\
  \public function usort($c){\
    \usort($this->a,$c);\
  \}\
  \public function jsonSerialize(){\
    \return $this->a;\
  \}\
\}?>"

genPkg :: (String -> String) -> String -> [(String, (Lang.Type, Semantics.Expression))] -> String
genPkg qual pkg = concat . map (uncurry $ genDeclaration qual pkg)

gen :: [String] -> [(String, [(String, (Lang.Type, Semantics.Expression))])] -> String
gen phps pkgs = concat phps ++ preamble qualified ++ concat (map (uncurry $ genPkg (quals Map.!)) pkgs)
  where qualified = map (("$z0" ++) . pad (length $ show $ length pkgs) '0' . show) $ number pkgs
        quals = Map.fromList $ zip (map fst pkgs) qualified
